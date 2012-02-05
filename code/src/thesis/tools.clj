(ns thesis.tools
  (:require
   [clojure.string :as string])
  (:import
   java.lang.Character))

(defn =any? [many one]
  "returns true if one = any of the items in the seq many"
  (true? (some #(= one %) many)))

(defn fix-dotty [dotstr]
  "replaces <= with a better symbol"
  (-> dotstr
      (string/replace  "<=" " \u2264")
      (string/replace  "\">" "\" >")))

(defn write-dtree [classifier path]
  (spit path (fix-dotty (.graph classifier))))

