(ns thesis.train-verbs
  (:require
   [thesis.data :as data]
   [thesis.verb :as verb]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats]))

(defn load-all-corpora-parses [L1]
  "return a seq of seqs of trees of a given L1. (instances->sentences)"
  (let [corpora (filter #(= (:L1 %) L1) data/*all-corpora*)]
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
                      verb/*all-tense-aspect-names*)
           )
    ))

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

(defn make-classifier-from-all-corpora []
  "makes a classifier trained on all of the corpora"
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