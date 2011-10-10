(ns thesis.AttributeVoting
  (:gen-class
   :name thesis.AttributeVoting
   :extends weka.classifiers.Classifier
   :state state
   :init init
   :constructors {[] []})
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
    (set-slot! "data-keys" (doall (last inverted)))
    (set-slot! "class-values" (doall (keys (mld/dataset-class-labels dataset))))))

(defn find-class-from-attr [attr-vec class-vec]
  ;;takes a list of difference between two instances attribute (training and to-be-classified instances) and
  ;;determine which has the closest (smallest numerically) match. the second argument is the labels
  ;;corresponding to the training instances
  (let [zipped (map #(vector %1 %2) class-vec attr-vec)
        ties (remove (fn [[class diff]]
                       (some #(< % diff) attr-vec))
                     zipped)
        tie-freqs (frequencies ties)
        equally-freq-ties (remove (fn [[t freq]]
                                    (some #(> (val %) freq) tie-freqs))
                                  tie-freqs)]
    (if-not (> (count equally-freq-ties) 1)
      (first (key (first equally-freq-ties))))))

(defn find-class-from-attr [attr-vec class-vec]
  ;;takes a list of difference between two instances attribute (training and to-be-classified instances) and
  ;;determine which has the closest (smallest numerically) match. the second argument is the labels
  ;;corresponding to the training instances
  (let [zipped (map #(vector %1 %2) class-vec attr-vec)
        labels (distinct class-vec)
        per-label (map (fn [label]
                         (filter (fn [[class attr]] (= class label)) zipped))
                       labels)
        summed (map #(reduce (fn [[class attr1] [_ attr2]]
                              [class (+ attr1 attr2)]) %)
                    per-label)]
    (first
     (reduce (fn [[class1 attr1] [class2 attr2]]
               (if (< attr1 attr2)
                 [class1 attr1]
                 [class2 attr2]))
             summed))
    ))

(defn probabilities-for-classes [class-vec class-labels]
  (let [class-vec (map keyword (remove nil? class-vec))
        v-count (count class-vec)
        l-count (count class-labels)
        v-freq (frequencies class-vec)]
    (map double
         (if (zero? v-count)
           (take l-count (repeat 0.0))
           (map (fn [class]
                  (if (contains? v-freq class)
                    (/ (v-freq class) v-count)
                    0.0))
                class-labels)))))

(defn -distributionForInstance [this inst]
  (let [attrs-to-classify (butlast (mld/instance-to-vector inst))
        class-values (get-slot "class-values")
        attr-results
        (for [attr-vec (get-slot "training-data")]
          (let [diffs (map #(Math/abs (- %1 %2)) attr-vec attrs-to-classify)]
            (find-class-from-attr diffs (get-slot "data-keys"))))]
    (into-array Double/TYPE (probabilities-for-classes attr-results class-values))))