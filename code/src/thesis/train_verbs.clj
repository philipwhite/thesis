(ns thesis.train-verbs
  (:require
   [thesis.data :as data]
   [thesis.verb :as verb]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats]
   [thesis.tools :as tools])
  (:import
   weka.classifiers.trees.J48
   weka.classifiers.trees.RandomForest
   java.io.File
   java.util.HashMap
   java.util.ArrayList))

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

(defn get-raw-data-for-all-corpora []
  "returns a seq of seqs of verb informations maps"
  (let [es (load-all-corpora-parses :es)
        en (load-all-corpora-parses :en)]
    (concat
     (for [inst es]
       [:es (mapcat verb/extract-verbs inst)])
     (for [inst en]
       [:en (mapcat verb/extract-verbs inst)]))))

;;;MODALS

(comment
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
     ds)))

(def *classifiers* [[:decision-tree :c45]
                    [:decision-tree :boosted-stump]
                    [:decision-tree :random-forest]
                    [:decision-tree :rotation-forest]
                    [:bayes :naive]
                    [:neural-network :multilayer-perceptron]
                    [:support-vector-machine :smo]
                    [:regression :logistic]
                    [:regression :pace]] )

(comment
  (defn test-modals []
   (let [ds (make-modal-dataset-from-all-corpora)]
     (doseq [[cl1 cl2] *classifiers*]
       (let [cl (mlc/make-classifier cl1 cl2)]
         (mlc/classifier-train cl ds)
         (mlc/classifier-evaluate cl :cross-validation ds 10))))))

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

(comment
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
     ds)))

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

(defn- dump-raw-dataset [ds]
  (spit "../data/raw-verb-data.txt" (pr-str ds)))

(defn- load-raw-dataset []
  (read-string (slurp "../data/raw-verb-data.txt")))



(def *basic-attributes-funcs*
  [["not" #(if (:negative? %) 1 0)]
   ["modal" #(count (:modal %))]
   ["progressive" #(if (:progressive? %) 1 0)]
   ["past" #(if (:past? %) 1 0)]
   ["perfect" #(if (:perfect? %) 1 0)]
   ["passive" #(if (:passive? %) 1 0)]
   ["quasimodal" #(if (empty? (:quasimodal %)) 0 1)]
   ["do" #(if (empty? (:operator %)) 0 1)]])

(def *basic-attributes*
  (vec (map first *basic-attributes-funcs*)))

(defn extract-basic-attr-from-verb [verb-map]
  (vec
   (for [[a f] *basic-attributes-funcs*]
     (f verb-map))))

(defn make-basic-attr-inst [raw-inst]
  "takes a seq of verb maps, calls extract-basic-attr-from-verb on each, sums them, and converts to relative freqs"
  (conj
   (vec (let [data (second raw-inst)
              n (float (count data))
              v (map extract-basic-attr-from-verb data)
              v (reduce #(map + %1 %2) v)
              v (map #(/ % n) v)]
          v))
   ((first raw-inst) {:es "es" :en "en"})))

(defn make-basic-attr-dataset [raw-data]
  "returns a dataset with the attributes *basic-attributes"
  (let [ds (mld/make-dataset "Combined Verb"
                             (concat *basic-attributes*
                                     [{"L1" ["es" "en"]}])
                             100)]

    (mld/dataset-set-class ds "L1")
    (doseq [raw raw-data]
      (mld/dataset-add ds (make-basic-attr-inst raw)))
    ds))

(defn raw-modals [rawdata]
  "returns a vector of all modal and quasimodals found in the raw data"
  (vec
   (distinct
    (remove empty?
            (mapcat (fn [[L1 rd]]
                      (mapcat (fn [verb] (conj (:modal verb)
                                               (:quasimodals verb))) rd)) rawdata)))))

(defn normalize-modal [m]
  "takes a raw-modal can return the base form"
  (cond
   (= "'d" m) "would"
   (= "'ll" m) "will"
   (string? m) m
   (some #(= (first m) %) ["am" "are" "is" "was" "were"]) (assoc (vec m) 0 "be")
   (some #(= (first m) %) ["has" "had"]) (assoc (vec m) 0 "have")
   (some #(= (first m) %) ["dares" "dared"]) (assoc (vec m) 0 "dare")
   (some #(= (first m) %) ["needs" "needed"]) (assoc (vec m) 0 "need")
   :else (vec m)))

(defn all-modals [rawdata]
  (vec
    (distinct
     (map normalize-modal (raw-modals rawdata)))))

(defn all-modals-attrs [rawdata]
  "returns a vector of all modal and quasimodals found in the raw data"
  (vec
   (distinct
    (for [m (map normalize-modal (raw-modals rawdata))]
      (if (or (vector? m) (seq? m))
        (reduce #(str %1 " " %2) m)
        m)))))

(defn extract-modals-from-verb [verb]
  (remove empty? (conj (:modal verb) (:quasimodals verb))))

(defn count-modals-in-verb [verb all]
  (vec
   (let [em (map normalize-modal (extract-modals-from-verb verb))]
     (for [m all]
       (if-let [result (some #(if (= m %) 1) em)]
         result 0)))))

(defn make-modal-inst [raw-inst all]
  (conj
   (vec (let [data (second raw-inst)
              n (float (count data))
              v (map  #(count-modals-in-verb % all) data)
              v (reduce #(doall (map + %1 %2)) v)
              v (map #(/ % n) v)]
          v))
   ((first raw-inst) {:es "es" :en "en"})))

(defn make-modal-dataset [raw-data]
  (let [all-mod (all-modals raw-data)
        all-mod-attr (all-modals-attrs raw-data)
        ds (mld/make-dataset "Modals"
                             (concat all-mod-attr
                                   [{"L1" ["es" "en"]}])
                             (count raw-data))]

    (mld/dataset-set-class ds "L1")
    (doseq [raw raw-data]
      (mld/dataset-add ds (make-modal-inst raw all-mod)))
    ds))

(def *modal-phrasal-pairs*
  [["will" [["be" "going" "to"]]]
   ["must" [["have" "to"] ["have" "got" "to"] ["need" "to"]]]
   ["should" [["ought to"] ["be" "supposed" "to"] ["be" "obliged" "to"]]]
   ["can" [["be" "able" "to"]]]])

(defn verb-has-modal [verb modal]
  "returns 1 or 0 to indicate true or false"
  (let [modals (map normalize-modal (extract-modals-from-verb verb))]
    ({true 1 nil 0}
     (some #(= modal %) modals))))

(defn get-modal-phrasal-count-for-verb [verb]
  (for [[modal phrasals] *modal-phrasal-pairs*]
    [(verb-has-modal verb modal) (reduce + (map (partial verb-has-modal verb) phrasals))]))

(defn get-modal-phrasal-freq [inst]
  (let [sum (reduce #(doall (map (fn [a b] (doall (map + a b))) %1 %2))
                    (map get-modal-phrasal-count-for-verb (second inst)))]
    (conj
     (vec
      (for [[m p] sum]
        (let [s (float (+ m p))]
          (if (zero? s)
            0.0
            (/ (- m p) s)))))
     ((first inst) {:es "es" :en "en"}))))

(defn make-modal-phrasal-dataset [raw-data]
  (let [ds (mld/make-dataset "Modals Pairs"
                             ["will" "must" "should" "can" {"L1" ["es" "en"]}]
                             (count raw-data))]
    (mld/dataset-set-class ds "L1")
    (doseq [raw raw-data]
      (mld/dataset-add ds (get-modal-phrasal-freq raw)))
    ds))

(defn get-high-freq-verb [verb]
  (verb/*high-freq-verb-inflections-map* (first (:verb verb))))

(defn get-high-freq-inst [raw]
  (let [[L1 data] raw
        num-verbs (count data)
        final-count (loop [[verb & more] data
                           counts (apply hash-map (interleave verb/*high-freq-verbs* (repeat 0)))]
                      (if verb
                        (if-let [found-verb (get-high-freq-verb verb)]
                          (recur more (update-in counts [found-verb] #(+ % 1)))
                          (recur more counts))
                        counts))]
    (conj
     (vec (map #(/ % (float num-verbs)) (map final-count verb/*high-freq-verbs*)))
     (L1 {:es "es" :en "en"}))))

(defn make-high-freq-dataset [raw-data]
  (let [ds (mld/make-dataset "High Freq Verbs"
                             (conj verb/*high-freq-verbs*
                              {"L1" ["es" "en"]})
                             (count raw-data))]
    (mld/dataset-set-class ds "L1")
    (doseq [raw raw-data]
      (mld/dataset-add ds (get-high-freq-inst raw)))
    ds))

(defn make-combined-dataset [raw-data]
  (let [all-mod (all-modals raw-data)
        all-mod-attr (all-modals-attrs raw-data)
        ds (mld/make-dataset "Combined Verbs"
                             (concat
                              *basic-attributes*
                              all-mod-attr
                              ["will*" "must*" "should*" "can*"]
                              verb/*high-freq-verbs*
                              [{"L1" ["es" "en"]}])
                             (count raw-data))]
    (mld/dataset-set-class ds "L1")
    (doseq [raw raw-data]
      (mld/dataset-add ds (concat
                           (drop-last (make-basic-attr-inst raw))
                           (drop-last (make-modal-inst raw all-mod))
                           (drop-last (get-modal-phrasal-freq raw))
                           (get-high-freq-inst raw))))
    ds))

(defn train-and-test [dataset]
  (let [cl (RandomForest.)]
    (.setNumTrees cl 100)
    ;(.setConfidenceFactor cl 0.0065)
    (println (mlc/classifier-train cl dataset))
    (let [r (mlc/classifier-evaluate cl :cross-validation dataset 20)]
      (r :evaluation-object))))

(defn train-and-test [dataset]
  (let [cl (J48.)]
    ;(.setConfidenceFactor cl 0.0065)
    ;(.setNumTrees cl 100)
    (println (mlc/classifier-train cl dataset))
    cl))

(defn J48-classifier [dataset]
  (let [cl (J48.)]
    (.setConfidenceFactor cl 0.0065)
    (mlc/classifier-train cl dataset)))