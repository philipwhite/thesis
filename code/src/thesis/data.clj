;;;This file contains code to access or manipulate the files in thesis/data

(ns thesis.data
  (:require
   [clojure.string :as string]
   [clojure.java.io :as io])
  (:import
   [java.io BufferedReader FileReader]))

(defn- load-csv-lines [path]
  "returns a sequence of sequences representing the lines and cells of a csv file"
  (let [rdr (BufferedReader. (FileReader. path))]
    (for [line (line-seq rdr)]
      (string/split line #","))))

(defn- load-csv [path]
  "returns a sequence of maps that contains all of the cells in the csv file at path, with the rows (skipping the first) shown as the maps and the elements of the map being the cells in that row, with the key given by the column heading of that cell"
  (let [csv (load-csv-lines path)
        keylist (map
		 (fn [x]
		   (let [x (string/replace (string/trim x) ":" "")];trim whitespace and remove : characters
		     (keyword x)))
		 (first csv))]
    (for [values (rest csv)] (zipmap keylist values))))

(def *micusp-directory* "../data/micusp/")
(def *micusp-keyfile* (str *micusp-directory* "micusp_papers.csv"))

(defn download-micusp []
  "Downloads the files listed in the 'PAPER ID' column of the *micusp-keyfile* into *micusp-directory*"
  (let [micusp-keys (load-csv *micusp-keyfile*)]
    (doseq [{fname (keyword "PAPER ID")} micusp-keys]
      (let [src (str "http://search-micusp.elicorpora.info/search/viewPaper/" fname ".pdf")
	    dst (str *micusp-directory* fname ".pdf")]
	(println src)
	(with-open [is (io/input-stream src)
		    os (io/output-stream dst)]
	  (io/copy is os))
	(print ".")))))