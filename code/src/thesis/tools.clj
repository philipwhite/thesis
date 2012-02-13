(ns thesis.tools
  (:require
   [clojure.string :as string])
  (:import
   java.lang.Character
   java.text.DecimalFormat))

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

