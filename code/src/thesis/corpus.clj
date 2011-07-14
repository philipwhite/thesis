(ns thesis.corpus
  (:use alex-and-georges.debug-repl) 
  (:require
   [clojure.string :as string])
  (:import
   [java.io BufferedReader FileReader]
   [edu.stanford.nlp.process DocumentPreprocessor]
   [edu.stanford.nlp.process WordTokenFactory]))

(if user/at-school?
  (def wricle-base-path "/home/school/Thesis/WricleCorpusv13/")
  (def wricle-base-path "/Users/philip/Work/MSU/Thesis/WricleCorpusv13/"))

(def wricle-key-path (str wricle-base-path "Wricle719.csv"))


(defn- load-wricle-key-lines []
  (let [rdr (BufferedReader. (FileReader. wricle-key-path))]
    (for [line (line-seq rdr)]
      (string/split line #","))))

(defn load-wricle-key []
  "returns a sequence of maps that describe each file"
  (let [csv (load-wricle-key-lines)
        keylist (map
                  (fn [x]
                    (let [x (string/replace (string/trim x) ":" "")];trim whitespace and remove : characters
                      (keyword x)))
                  (first csv))]
    (for [values (rest csv)] (zipmap keylist values))))

(defn wricle-files [key-file]
  "returns a lazy sequence of strings"
  (let [factory (WordTokenFactory.)
	get-sentences (fn [path]
			(seq
			  (DocumentPreprocessor.
			    (str wricle-base-path "files/" (:ESSAY_FILENAME path)))))]
    (map get-sentences key-file)))