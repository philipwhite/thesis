(ns thesis.tools
  (:require
   [clojure.string :as string]
   [clj-ml.classifiers :as mlc])
  (:import
   java.lang.Character
   java.text.DecimalFormat
   java.util.Random
   java.lang.Math
   weka.core.Instances
   weka.classifiers.Classifier
   weka.classifiers.trees.J48))

(defn =any? [many one]
  "returns true if one = any of the items in the seq many"
  (true? (some #(= one %) many)))

(defn- num->percent [num-in-str]
  (let [df (DecimalFormat/getPercentInstance)
        num (read-string num-in-str)]
    (.setMinimumFractionDigits df 4)
    (.setMaximumFractionDigits df 4)
    (let [per-in-str (.format df num)]
      (string/replace per-in-str "%" ""))))

(defn- fix-dotty [dotstr]
  "replaces <= with a better symbol"
  (let [dotstr (-> dotstr
                   (string/replace-first "\n" "\nmargin=0\n")
                   (string/replace  "<=" " \u2264")
                   (string/replace  "\">" "\" >")
                   (string/replace ".0/" "/")
                   (string/replace ".0)" ")"))
        numbers (distinct (re-seq #"0\.[0-9]+" dotstr))]
    (loop [str dotstr
           [to-replace & more] numbers]
      (if (nil? to-replace)
        str
        (recur (string/replace str to-replace (num->percent to-replace))
               more)))))

(defn write-dtree [classifier path]
  (spit path (fix-dotty (.graph classifier))))

(defn write-classifier-dtree [dataset path]
  (let [cl (J48.)]
    (.setConfidenceFactor cl 0.0065)
    (mlc/classifier-train cl dataset)
    (write-dtree cl path)))


(defn cv-conf-interval [cv-results]
  (let [corrects (map (fn [[a b]] (if (== a b) 1 0)) cv-results)]
    (let [n (count cv-results)
          p (/ (reduce + corrects) (float n))
          err (* 1.96 (Math/sqrt (/ (* p (- 1.0 p)) n)))]
      [p err])))

(defn cv-accuracy-table [cv-results]
  (let [ind-tables
        (map (fn [[class-as actual]]
               (cond
                (and (== actual 0) (== class-as 0))
                [1 0 0 0]
                (and (== actual 0) (== class-as 1))
                [0 1 0 0]
                (and (== actual 1) (== class-as 1))
                [0 0 1 0]
                (and (== actual 1) (== class-as 0))
                [0 0 0 1]
                :else
                (throw "Error in cv-accuracy-table")))
             cv-results)]
    (let [table (reduce (fn [a b] (map + a b)) ind-tables)]
      [(/ (nth table 0) (float (+ (nth table 0) (nth table 1))))
       (/ (nth table 2) (float (+ (nth table 2) (nth table 3))))])))

(defn cross-validate [orig-dataset num-folds orig-classifier]
  "This performs cross-validation using the classifier and returns a map with accuracy
information based on the binomial distribution"  
  (let [dset (Instances. orig-dataset)] ;copy the dataset
    (.randomize dset (Random.))
    (let [fold-results
          (for [f (range num-folds)]
             (let [train-set (.trainCV dset num-folds f)
                   test-set (.testCV dset num-folds f)
                   classifier (Classifier/makeCopy orig-classifier)]
               (.buildClassifier classifier train-set)
               (for [n (range (.numInstances test-set))]
                 (let [i (.instance test-set n)]
                   [(int (.classifyInstance classifier i))
                    (int (.classValue i))]))))]
      ;;at this point fold results contains a seq of the same length as the fold
      ;;each of the elements are sequences in turn which contains pairs (vectors)
      ;;or integers, the first (1 or 0), indicating the classification, the second
      ;;indicating the true class
      ;; lets flatten it one level
      (let [fold-results (reduce concat fold-results)
            ci (cv-conf-interval fold-results)]
        {:per-class-correct (cv-accuracy-table fold-results)
         :accuracy (first ci)
         :conf-interval [(- (first ci) (second ci)) (+ (first ci) (second ci))]}))))
