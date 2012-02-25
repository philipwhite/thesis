(ns thesis.train-verbs
  (:require
   [thesis.data :as data]
   [thesis.verb :as verb]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats])
  (:import
   weka.classifiers.trees.J48
   java.io.File))

(defn load-all-corpora-parses [L1]
  "return a seq of seqs of trees of a given L1. (instances->sentences)"
  (let [corpora ({:es data/*es-corpora* :en data/*en-corpora*} L1)]
    (mapcat (fn [corpus-info]
              (let [corpus (:corpus corpus-info)]
                (map (fn [filename]
                       (println corpus " " filename)
                       (data/load-parse corpus filename))
                     (:filenames corpus-info))))
            corpora)))

(defn relative-tense-aspect-freqs [parses]
  "takes a sequence of parses. returns a map where the keys are the tense/aspect names and the values are the relative frequencies of that particular tense/aspect"
  (let [tenses (mapcat verb/extract-verbs parses)
        tense-count (count tenses)
        tense-names (map verb/get-tense-aspect-name tenses)
        tense-freqs (frequencies tense-names)
        tense-rel-freqs (zipmap (keys tense-freqs)
            (map #(vector (float (/ % tense-count)))
                 (vals tense-freqs)))]
    
    ;;now add zeros for any unused tenses
    (apply merge (map (fn [tense]
                        (if (contains? tense-rel-freqs tense)
                          {tense (tense-rel-freqs tense)}
                          {tense [0.0]}))
                      verb/*all-tense-aspect-names*))))

(defn relative-tense-aspect-freqs-2 [tenses]
  "takes a sequence of verb mappings. returns a map where the keys are the tense/aspect names and the values are the relative frequencies of that particular tense/aspect. Same as the function with the same name (w/0 the -2) except that it expects verb/extract-verb to have already been called"
  (let [tense-count (count tenses)
        tense-names (map verb/get-tense-aspect-name tenses)
        tense-freqs (frequencies tense-names)
        tense-rel-freqs (zipmap (keys tense-freqs)
            (map #(vector (float (/ % tense-count)))
                 (vals tense-freqs)))]
    
    ;;now add zeros for any unused tenses
    (apply merge (map (fn [tense]
                        (if (contains? tense-rel-freqs tense)
                          {tense (tense-rel-freqs tense)}
                          {tense [0.0]}))
                      verb/*all-tense-aspect-names*)
           )))

(defn run-tense-aspect-t-test []
  (let [es-freqs (apply (partial merge-with concat)
                         (map relative-tense-aspect-freqs
                               (load-all-corpora-parses :es)))
        en-freqs (apply (partial merge-with concat)
                         (map relative-tense-aspect-freqs
                              (load-all-corpora-parses :en)))]
    (spit "../output/tense-aspect-t-test.txt"
          (with-out-str
            (doseq [tense verb/*all-tense-aspect-names*]
              ;;incanter doesn't like it if all zeros are passed to it
              (if (some #(not= 0 %) (concat (es-freqs tense) (en-freqs tense)))
                (let [test-results (stats/t-test (es-freqs tense)
                                                 :y (en-freqs tense))]
                  (print "\n*****\n"
                         tense "\n"
                         test-results "\n"))))))))

(defn make-tense-dataset []
  "Makes a dataset with numerical attributes of tense/aspects relative frequencies"
  (let [ds (mld/make-dataset "Tense-Aspects"
                             (conj verb/*all-tense-aspect-names*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-tense-dataset [ds tense-freqs L1]
  "adds an instance using the data found in the map tense-freqs"
  (mld/dataset-add ds (flatten (concat (map tense-freqs verb/*all-tense-aspect-names*)
                                       [({:en "en" :es "es"} L1)]))))

(defn make-tense-dataset-from-all-corpora []
  "makes a dataset using tense information from all of the corpora"
  (let [ds (make-tense-dataset)
        es-freqs (map relative-tense-aspect-freqs
                      (load-all-corpora-parses :es))
        en-freqs (map relative-tense-aspect-freqs
                      (load-all-corpora-parses :en))]
    (doseq [f es-freqs]
      (add-to-tense-dataset ds f :es))
    (doseq [f en-freqs]
      (add-to-tense-dataset ds f :en))
    ds))

;;;MODALS

(defn relative-modal-freqs [parses]
  "takes a sequence of parses. returns a map where the keys are the modals and the values are the relative frequencies of that particular modal"
  (let [modals (mapcat verb/extract-modals parses)
        modal-count (count modals)
        modal-freqs (frequencies modals)
        modal-rel-freqs (zipmap (keys modal-freqs)
                                (map #(float (/ % modal-count))
                                     (vals modal-freqs)))]
    
    ;;now add zeros for any modals
    (apply merge (map (fn [m]
                        (if (contains? modal-rel-freqs m)
                          {m (modal-rel-freqs m)}
                          {m 0.0}))
                      verb/*all-modals*))))

(defn make-modal-dataset []
  "Makes a dataset with numerical attributes of modal relative frequencies"
  (let [ds (mld/make-dataset "Modals"
                             (conj verb/*all-modals*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-modal-dataset [ds modal-freqs L1]
  "adds an instance using the data found in the map modal-freqs"
  (mld/dataset-add ds (concat (map modal-freqs verb/*all-modals*)
                              [({:en "en" :es "es"} L1)])))

(defn make-modal-dataset-from-all-corpora []
  "makes a dataset using modal relative frequencies all of the corpora"
  (let [ds (make-modal-dataset)
        es-freqs (map relative-modal-freqs
                      (load-all-corpora-parses :es))
        en-freqs (map relative-modal-freqs
                      (load-all-corpora-parses :en))]
    (doseq [m es-freqs]
      (add-to-modal-dataset ds m :es))
    (doseq [m en-freqs]
      (add-to-modal-dataset ds m :en))
    ds))

(def *classifiers* [[:decision-tree :c45]
                    [:decision-tree :boosted-stump]
                    [:decision-tree :random-forest]
                    [:decision-tree :rotation-forest]
                    [:bayes :naive]
                    [:neural-network :multilayer-perceptron]
                    [:support-vector-machine :smo]
                    [:regression :logistic]
                    [:regression :pace]] )

(defn test-modals []
  (let [ds (make-modal-dataset-from-all-corpora)]
    (doseq [[cl1 cl2] *classifiers*]
      (let [cl (mlc/make-classifier cl1 cl2)]
        (mlc/classifier-train cl ds)
        (mlc/classifier-evaluate cl :cross-validation ds 10)))))

;;HIGH FREQUENCE VERBS

(defn relative-hf-verb-freqs [parses]
  "takes a sequence of parses. returns a map where the keys are the high freq verb base form and the values are the relative frequencies of that particular high freq verb"
  (let [hf-verbs (mapcat verb/extract-high-freq-verbs parses)
        hf-verb-count (count hf-verbs)
        hf-verb-freqs (frequencies hf-verbs)
        hf-verb-rel-freqs (zipmap (keys hf-verb-freqs)
                                (map #(float (/ % hf-verb-count))
                                     (vals hf-verb-freqs)))]
    
    ;;now add zeros for any hf-verb that doesn't appear
    (apply merge (map (fn [hfv]
                        (if (contains? hf-verb-rel-freqs hfv)
                          {hfv (hf-verb-rel-freqs hfv)}
                          {hfv 0.0}))
                      verb/*high-freq-verbs*))))

(defn make-hf-verb-dataset []
  "Makes a dataset with numerical attributes of hf-verb relative frequencies"
  (let [ds (mld/make-dataset "High Frequency Verbs"
                             (conj verb/*high-freq-verbs*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-hf-verb-dataset [ds hf-verb-freqs L1]
  "adds an instance using the data found in the map hf-verb-freqs"
  (mld/dataset-add ds (concat (map hf-verb-freqs verb/*high-freq-verbs*)
                              [({:en "en" :es "es"} L1)])))

(defn make-hf-verb-dataset-from-all-corpora []
  "makes a dataset using hf-verb relative frequencies of all the corpora"
  (let [ds (make-hf-verb-dataset)
        es-freqs (map relative-hf-verb-freqs
                      (load-all-corpora-parses :es))
        en-freqs (map relative-hf-verb-freqs
                      (load-all-corpora-parses :en))]
    (doseq [m es-freqs]
      (add-to-hf-verb-dataset ds m :es))
    (doseq [m en-freqs]
      (add-to-hf-verb-dataset ds m :en))
    ds))


;;COMBINED TENSE/MODALS/HIGH FREQ VERBS

(def *combined-attribute-names* (concat verb/*all-tense-aspect-names*
                                        verb/*high-freq-verbs*
                                        verb/*all-modals*))

(defn relative-combined-freqs [parses]
  "takes a sequence of parses. returns a map where the keys are the combined attribute names and the values are the relative frequencies of that particular attribute"
  (let [verbs (mapcat verb/extract-verbs parses)
        
        tense-names (map verb/get-tense-aspect-name verbs)
        modals (mapcat verb/extract-modals parses)
        hf-verbs (mapcat verb/extract-high-freq-verbs-2 verbs)

        attributes (concat tense-names modals hf-verbs)
        attribute-count (count attributes)
        attribute-freqs (frequencies attributes)
        
        attribute-rel-freqs (zipmap (keys attribute-freqs)
                                (map #(float (/ % attribute-count))
                                     (vals attribute-freqs)))]
    
    ;;now add zeros for any attribute that doesn't appear
    (apply merge (map (fn [attr]
                        (if (contains? attribute-rel-freqs attr)
                          {attr (attribute-rel-freqs attr)}
                          {attr 0.0}))
                      *combined-attribute-names*))))

(defn make-combined-dataset []
  "Makes a dataset with numerical attributes of tense, modals, and high freq
verbs"
  (let [ds (mld/make-dataset "Combined Verb"
                             (concat *combined-attribute-names*
                                     [{"L1" ["es" "en"]}])
			  100)]

    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-combined-dataset [ds combined-freqs L1]
  "adds an instance using the data found in the map combined-freqs"
  (mld/dataset-add ds (concat (map combined-freqs *combined-attribute-names*)
                              [({:en "en" :es "es"} L1)])))

(defn make-combined-dataset-from-all-corpora []
  "makes a dataset using combined attribute relative frequencies of all the corpora"
  (let [ds (make-combined-dataset)
        es-freqs (map relative-combined-freqs
                      (load-all-corpora-parses :es))
        en-freqs (map relative-combined-freqs
                      (load-all-corpora-parses :en))]
    (doseq [m es-freqs]
      (add-to-combined-dataset ds m :es))
    (doseq [m en-freqs]
      (add-to-combined-dataset ds m :en))
    ds))

(defn train-and-test [dataset]
  (let [cl (J48.)]
    
    (mlc/classifier-train cl dataset)
    (mlc/classifier-evaluate cl :cross-validation dataset 10)))

(defn- dump-dataset [ds path]
  (with-open [outp (-> (File. path)
                       java.io.FileOutputStream.
                       java.io.ObjectOutputStream.)]
    (.writeObject outp ds)))

(defn- load-dataset [path]
  (with-open [inp (-> (File. path)
                        java.io.FileInputStream.
                        java.io.ObjectInputStream.)]
    (.readObject inp)))

(defn- dump-tense-dataset [ds]
  (dump-dataset ds "../data/train-tense.dataset"))

(defn- load-tense-dataset []
  (load-dataset "../data/train-tense.dataset"))

(defn train-and-test [dataset]
  (let [cl (J48.)]
                                        ;(.setNumTrees cl 3)
    (println (mlc/classifier-train cl dataset))
    (let [r (mlc/classifier-evaluate cl :cross-validation dataset 20)]
      (r :evaluation-object))
    ))