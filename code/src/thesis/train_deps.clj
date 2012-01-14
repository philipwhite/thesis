

(ns thesis.train-deps
   (:import
    edu.stanford.nlp.trees.EnglishGrammaticalStructure
    [weka.core Instances Attribute]
    [weka.classifiers.trees RandomForest J48]
    java.util.Vector
    [weka.attributeSelection InfoGainAttributeEval ClassifierSubsetEval GeneticSearch RaceSearch Ranker ChiSquaredAttributeEval]
    [weka.core SelectedTag])
   
   (:require
    [thesis.parse :as parse] ()
    [thesis.data :as data]
    [clj-ml.data :as mld]
    [clj-ml.classifiers :as mlc]
    [incanter.stats :as stats]
    [incanter.core :as incanter]))

(def *all-reln*
  (vec (.keySet (EnglishGrammaticalStructure/shortNameToGRel))))

(defn- relations [deps]
  "takes a seq of seqs of dependencies and returns a seq of seqs of just the relationship names"
  (map (fn [x] (map #(.getShortName (.reln %)) x)) deps))

(defn- count-equals [sqnc itm]
  "returns the number of items in sqnc equal to itm"
  (reduce + (map #(if (= % itm) 1 0) sqnc)))

(defn count-reln [deps reln]
  "taking a seq of seqs of deps, returns a number indicating how many times a particular reln appears"
  (reduce + (map #(count-equals % reln) (relations deps))))

(defn make-num-reln-dataset []
  "Makes a dataset with numerical attributes of dependency relations (just relations) without instances"
  (let [ds (mld/make-dataset "Dependencies"
			  (conj *all-reln* {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-reln-dataset [ds deps L1]
  "adds an instance using the data found in deps"
  (let [tot-reln-count (float (reduce + (map count deps)))
	counts (vec (->> *all-reln*
			 (map #(count-reln deps %))
			 (map #(/ % tot-reln-count))))]
    (mld/dataset-add ds (conj counts L1))))



(defn make-reln-dataset-with-samples [corpus en-samples es-samples]
  "Pass corpus title and 2 seq of titles, first for en 2nd for es."
  (let [ds (make-num-reln-dataset)
	load-samples (fn [samples]
		       (map #(data/load-deps corpus %) samples))]
    (doseq [d (load-samples en-samples)]
      (println "+")
      (add-to-reln-dataset ds d "en"))
    (doseq [d (load-samples es-samples)]
      (println "+")
      (add-to-reln-dataset ds d "es"))
    ds))

(defn add-dataset-to-dataset [add-to add-from]
  "adds the instances from add-from to add-to and returns add-to"
  (map #(mld/dataset-add add-to %) (mld/dataset-seq add-from)))

(def *min-dep-num* 0) ;any file with fewer than this number of deps will be skipped

(defn make-reln-dataset-with-samples [sample-maps]
  "argument should be a seq of maps with keys :corpus,:filenames,:L1.
the values for corpus and L1 should be keywords, :micusp, etc, :es or :en
and for :filenames the value should be a list of strings, without a type suffix"
  (let [ds (make-num-reln-dataset)]
    (doseq [{corpus :corpus filenames :filenames L1 :L1}
            sample-maps]
      (doseq [filename filenames]
        (let [d (data/load-deps corpus filename)
              L1-str (L1 {:es "es" :en "en"})]
          (if (< (count d) *min-dep-num*)
            (println "Skipping" filename "in" corpus "--" (count d) "deps.")
            (do
              (add-to-reln-dataset ds d L1-str)
              (println "Added" filename "from" corpus "--" (count d) "deps."))))))
    ds))


(defn test-for-best-min-dep-num [sample-maps mins]
  "This function tests to see what value *min-dep-num* should be set to."
  (def *test-results* [])
  (doseq [m mins]
    (with-bindings {#'*min-dep-num* m}
      (println "*min-dep-num* =" *min-dep-num*)
      (let [dataset (make-reln-dataset-with-samples data/*all-corpora*)
            classifier (mlc/make-classifier :neural-network :multilayer-perceptron)]
        (mlc/classifier-train classifier dataset)
        (let [total (loop [count 10
                           sum 0]
                      (if (> count 0)
                        (let [{correct :percentage-correct}
                              (mlc/classifier-evaluate classifier
                                                       :cross-validation
                                                       dataset 20)]
                          (recur (- count 1) (+ sum correct)))
                        sum))]
          (def *test-results* (conj *test-results* [m (/ total 10)]))
          (println (last *test-results*)))))))

(defn run-root-attr-trim-experiment 
  "This function makes an unpruned J48 tree using the data in dataset,
evaluates it using cross validation, then removes the attribute from dataset that is the basis of the initial decision in the tree and calls itself again, decrementing depth. returns when depth is zero. does not modify dataset"
  ([dataset depth]
     (run-root-attr-trim-experiment dataset depth
                               (mlc/make-classifier :decision-tree
                                                    :c45
                                                    {:unpruned true})))
  ([dataset depth classifier]
     (when (> depth 0)
       (mlc/classifier-train classifier dataset)
       (println classifier)
       (mlc/classifier-evaluate classifier :cross-validation dataset 20)
       (let [root-att (re-find #"(?<=^\[)[A-Za-z]+(?=:)"
                               (.prefix classifier))
             dataset-mod (Instances. dataset)]
         (->> root-att
              (.attribute dataset-mod)
              .index
              (.deleteAttributeAt dataset-mod))
         (recur dataset-mod (- depth 1) classifier)))))

(defn relative-dep-freqs [deps]
  "take a seq of seqs of dependencies. returns a map where the keys are the relation names and the values are the relative frequencies of that particular dependency (in a vector, to ease concating in run-t-test)"
  (let [dep-count (reduce + (map count deps))]
    (apply merge (map #(hash-map % [(float (/ (count-reln deps %) dep-count))])
                      *all-reln*))))

;;TODO does this method actuall return a seq of seqs of seqs or one level less?
(defn load-all-corpora-deps [L1]
  "return a seq of seqs of seqs of deps (instances->sentences->sentence->dep"
  (let [corpora (filter #(= (:L1 %) L1) data/*all-corpora*)]
    (mapcat (fn [corpus-info]
              (let [corpus (:corpus corpus-info)]
                (map (fn [filename]
                       (data/load-deps corpus filename))
                     (:filenames corpus-info))))
            corpora)))

(defn run-t-test [conf]
  (let [es-freqs (apply (partial merge-with concat)
                         (map relative-dep-freqs
                              (load-all-corpora-deps :es)))
        en-freqs (apply (partial merge-with concat)
                         (map relative-dep-freqs
                              (load-all-corpora-deps :en)))]
    (apply merge
           (for [reln *all-reln*]
             ;;incanter doesn't like it if all zeros are passed to it
             (if (some #(not= 0 %) (concat (es-freqs reln) (en-freqs reln)))
               {reln (stats/t-test (es-freqs reln) :y (en-freqs reln) :conf-level conf)})))))

(defn filter-t-test-results [tr]
  (apply merge
         (for [[reln results] tr]
           (let [[lower upper] (:conf-int results)
                 es-mean (:x-mean results)
                 en-mean (:y-mean results)]
             (if (or (and (< lower 0) (< upper 0))
                     (and (> lower 0) (> upper 0)))
               (if (or (> es-mean 0.05) (> en-mean 0.05))
                 {reln results}))))))

(defn train-and-test [dataset]
  (let [cl (J48.)]
    (.setBinarySplits cl true)
    ;;(.setMinNumObj cl 100)
    (.setSeed cl (System/currentTimeMillis))
    (mlc/classifier-train cl dataset)
    (mlc/classifier-evaluate cl :cross-validation dataset 10)
    cl))

(comment (defn eval-attributes [dataset]
  (let [attr-indexes (range 0 (.numAttributes dataset))
        evaluator (InfoGainAttributeEval.)]
    (.buildEvaluator evaluator dataset)
    (sort-by second #(if (> %1 %2) -1 1)
           (for [ai attr-indexes]
             [(.name (.attribute dataset ai))
              (.evaluateAttribute evaluator ai)])))))

(defn eval-attributes [dataset]
  (let [classifier (RandomForest.)
        eval (ClassifierSubsetEval.)
        searcher (GeneticSearch.)]
    (.setNumTrees classifier 100)
    (.setClassifier eval classifier)
    (.buildEvaluator eval dataset)
    (.setMaxGenerations searcher 100000)
    (.setPopulationSize searcher 100)
    (.setStartSet searcher (str 1 "-" (.numAttributes dataset)))
    ;(.setMutationProb searcher 0.05)
    (repeatedly 1 #(seq (do (.setSeed searcher (System/currentTimeMillis))
                             (.search searcher eval dataset))))))

(defn done-searching [result-map]
  "returns nil if not done searching, otherwise return a list that contains
the items that have occurred at least the min number of times"
  (let [dones
        (remove nil? (map (fn [[k v]] (if (> v 5) k)) result-map))]
    (if (> (count dones) 5)
      dones)))

(defn eval-attributes-1 [dataset]
  (let [classifier (RandomForest.)
        eval (ClassifierSubsetEval.)
        searcher (GeneticSearch.)]
    (.setNumTrees classifier 100)
    (.setClassifier eval classifier)
    (.buildEvaluator eval dataset)
    (.setMaxGenerations searcher 1000)
    (loop [result-map {}]
      (println result-map)
      (if-let [r (done-searching result-map)]
        r
        (let [part-r (seq (do (.setSeed searcher (System/currentTimeMillis))
                              (.search searcher eval dataset)))]
          (recur (apply (partial merge-with + result-map)
                        (for [k part-r] {k 1}))))))))


(defn eval-attributes [dataset]
  (let [classifier (J48.)
        eval (ChiSquaredAttributeEval.)
        searcher (Ranker.)]
    ;(.setNumTrees classifier 100)
    ;(.setClassifier eval classifier)
    (.buildEvaluator eval dataset)
    ;(.setRaceType searcher (SelectedTag. 0 (RaceSearch/TAGS_SELECTION)))
    (.setGenerateRanking searcher true)
    (.search searcher eval dataset)
    (.rankedAttributes searcher)))


(defn count-reln-in-corpora [reln]
  (doseq [corp data/*all-corpora*]
    (print (corp :corpus) " " (corp :L1)
           (apply + (for [fname (corp :filenames)]
                      (count-reln (data/load-deps (corp :corpus) fname) reln)))
           "\n\n")))

(defn count-reln-in-corpus [reln corp-key]
  
  (if-let [corp (some #(if (= (% :corpus) corp-key) %) data/*all-corpora*)]
    (print corp-key " " (corp :L1)
                 (apply + (for [fname (corp :filenames)]
                            (count-reln (data/load-deps corp-key fname) reln)))
                 "\n\n")
    (throw (.Exception "No such corpus"))))

