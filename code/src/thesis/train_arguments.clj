;;note 'arg' refers to the system that distinguishes anaphora from lexical NP, arg-struct refers
;;to just looking at different verb types in terms of the argument patterns they accept

(ns thesis.train-arguments
  (:require
   [thesis.data :as data]
   [thesis.arguments :as arg]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats]
   [incanter.core :as incanter]
   [clojure.string :as string])
  (:import
   weka.classifiers.trees.RandomForest))

(defn load-all-corpora-deps [L1]
  "return a seq of seqs of seqs of deps of a given L1. (instances->sentences)"
  (let [corpora ({:es data/*es-corpora* :en data/*en-corpora*} L1)]
    (mapcat (fn [corpus-info]
              (let [corpus (:corpus corpus-info)]
                (map (fn [filename]
                       (println corpus " " filename)
                       (data/load-deps corpus filename))
                     (:filenames corpus-info))))
            corpora)))

(defn case-ins-eq? [a b]
  (= (string/upper-case a) (string/upper-case b)))

(defn relative-arg-freqs-1 [args]
  "used by relative-arg-freqs, takes a list of argument attributes 'ao' 'aiO' etc, counts each type and divides that number by the frequency of tha particular structure"
  (let [structure-counts
        (reduce into
                (for [s arg/*all-argument-structures*]
                  {s (apply + (for [a args]
                                (if (case-ins-eq? a s) 1 0)))}))
        arg-freqs (frequencies args)
        arg-rel-freqs (zipmap (keys arg-freqs)
                              (for [[attr freq] arg-freqs]
                                (double (/ freq (structure-counts (string/lower-case attr))))))]
    arg-rel-freqs))

(defn relative-arg-freqs [deps]
  "takes a sequence of sequences of deps. returns a map where the keys are the arg structures and the values are the relative frequencies of that particular arg structure"
  (let [args (mapcat arg/extract-arguments deps)
        arg-rel-freqs (relative-arg-freqs-1 args)]
    
    ;;now add zeros for any args
    (apply merge (map (fn [a]
                        (if (contains? arg-rel-freqs a)
                          {a (arg-rel-freqs a)}
                          {a  0.0}
                          ;;{a [(arg-rel-freqs a)]};;replace above with this for t-test
                          ;;{a  [0.0]}
                          ))
                      arg/*all-argument-attributes*))))

(defn relative-arg-struct-freqs [deps]
  "ignores whether the arguments are anaphora and just looks at relative frequencies of the different
argument structures. there are six: s,ao,aio,aoc,p,pc"
  (let [args (map string/lower-case (mapcat arg/extract-arguments deps))
        arg-count (count args)
        arg-freqs (frequencies args)
        arg-rel-freqs (apply merge (for [[struct freq] arg-freqs]
                                     {struct (double (/ freq arg-count))}))]
    ;;now add zeros for any args that aren't used
    (apply merge (map (fn [a]
                        (if (contains? arg-rel-freqs a)
                          {a (arg-rel-freqs a)}
                          {a  0.0}))
                      arg/*all-argument-structures*))))

(defn make-arg-dataset []
  "Makes a dataset with numerical attributes of argument relative frequencies"
  (let [ds (mld/make-dataset "Arguments"
                             (conj arg/*all-argument-attributes*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn make-arg-struct-dataset []
  "Makes a dataset with numerical attributes of argument relative frequencies"
  (let [ds (mld/make-dataset "Arguments Structures"
                             (conj arg/*all-argument-structures*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-arg-dataset [ds arg-freqs L1]
  "adds an instance using the data found in the map arg-freqs"
  (mld/dataset-add ds (concat (map arg-freqs arg/*all-argument-attributes*)
                              [({:en "en" :es "es"} L1)])))

(defn add-to-arg-struct-dataset [ds arg-freqs L1]
  "adds an instance using the data found in the map arg-freqs"
  (mld/dataset-add ds (concat (map arg-freqs arg/*all-argument-structures*)
                              [({:en "en" :es "es"} L1)])))

(defn make-arg-dataset-from-all-corpora []
  "makes a dataset using arg relative frequencies all of the corpora"
  (let [ds (make-arg-dataset)
        es-freqs (map relative-arg-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-freqs (map relative-arg-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :en)))]
    (doseq [a es-freqs]
      (add-to-arg-dataset ds a :es))
    (doseq [a en-freqs]
      (add-to-arg-dataset ds a :en))
    ds))

(defn make-arg-struct-dataset-from-all-corpora []
  "makes a dataset using arg relative frequencies all of the corpora"
  (let [ds (make-arg-struct-dataset)
        es-freqs (map relative-arg-struct-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-freqs (map relative-arg-struct-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :en)))]
    (doseq [a es-freqs]
      (add-to-arg-struct-dataset ds a :es))
    (doseq [a en-freqs]
      (add-to-arg-struct-dataset ds a :en))
    ds))

(defn train-and-test [dataset]
  (let [cl (RandomForest.)]
    (.setNumTrees cl 100)
    (mlc/classifier-train cl dataset)
    (mlc/classifier-evaluate cl :cross-validation dataset 10)))

(defn run-arguments-t-test []
  (let [es-freqs (doall
                  (apply (partial merge-with concat)
                         (map relative-arg-freqs
                              (map (partial remove empty?) (load-all-corpora-deps :es)))))
        en-freqs (doall
                  (apply (partial merge-with concat)
                         (map relative-arg-freqs
                              (map (partial remove empty?) (load-all-corpora-deps :en)))))]
    (for [arg-pattern arg/*all-argument-attributes*]
      ;;incanter doesn't like it if all zeros are passed to it
      (if (some #(not= 0 %) (concat (es-freqs arg-pattern) (en-freqs arg-pattern)))
        (let [test-results (stats/t-test (es-freqs arg-pattern)
                                         :y (en-freqs arg-pattern))]
          (print "\n*****\n"
                 arg-pattern "\n"
                 test-results "\n")
          (into test-results {:arg-pattern arg-pattern}))))))

(defn filter-t-test-results [results]
  (->> results
       (remove (fn [{[lower upper] :conf-int}] (or (nil? lower) (nil? upper))))
       (remove (fn [{[lower upper] :conf-int}] (and (< lower 0) (> upper 0))))))

(defn print-t-test-results [results]
  (doseq [r (filter-t-test-results results)]
    (println (:arg-pattern r) " --> " (if (> (:y-mean r) (:x-mean r))
                                        "EN"
                                        "ES"))))

(defn get-ind-args [deps]
  "For a single instance of deps (multiple sentences), return all individual arguments"
  (let [args (mapcat arg/extract-arguments deps)]
    (flatten (for [a args] (map str (seq a))))))

(defn get-per-arg-counts []
  (let [es-args (mapcat get-ind-args
                     (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-args (mapcat get-ind-args
                        (map (partial remove empty?) (load-all-corpora-deps :en)))
        es-freqs (frequencies es-args)
        en-freqs (frequencies en-args)]
    {:en en-freqs :es es-freqs}))

(defn get-arg-struct-counts []
  (let [es-args (mapcat arg/extract-arguments
                     (mapcat (partial remove empty?) (load-all-corpora-deps :es)))
        en-args (mapcat arg/extract-arguments
                        (mapcat (partial remove empty?) (load-all-corpora-deps :en)))
        es-freqs (frequencies es-args)
        en-freqs (frequencies en-args)]
    {:en en-freqs :es es-freqs}))