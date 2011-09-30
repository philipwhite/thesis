(ns thesis.AttributeVoting
  (:gen-class
   :name thesis.AttributeVoting
   :extends weka.classifiers.Classifier
   :state state
   :init init
   :constructors {[] []})
  (:import
   java.util.HashMap)
  (:require 
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]))

(defmacro get-slot [key]
  `(.get (.state ~'this) ~key))

(defmacro set-slot! [key value]
  `(.put (.state ~'this) ~key ~value))

(defn -init []
  [nil (HashMap.)])

(defn -toString [this]
  (if-let [td (get-slot "training-data")]
    (str td)
    "Untrained"))

(defn -buildClassifier [this dataset]
  (let [ds-map (mld/dataset-as-maps dataset)
        inverted (for [attr (mld/attribute-names dataset)]
                   (map attr ds-map))]
    (set-slot! "training-data" (butlast inverted))
    (set-slot! "data-keys" (doall (last inverted))))
  (println (.toString this)))

(defn -distributionForInstance [this inst]
  (into-array Double/TYPE [0.0 1.0]))

(comment (let [attrs-to-classify (mld/instance-to-vector inst)
        attr-results
        (for [attr-vec (get-slot "training-data")]
          )]))