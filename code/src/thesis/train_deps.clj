

(ns thesis.train-deps
   (:import
    edu.stanford.nlp.trees.EnglishGrammaticalStructure)
   (:require
    [thesis.parse :as parse]
    [thesis.data :as data]
    [clj-ml.data :as mld]
    [clj-ml.classifiers :as mlc]))

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
  )