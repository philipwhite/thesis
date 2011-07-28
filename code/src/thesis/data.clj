;;;This file contains code to access or manipulate the files in thesis/data

(ns thesis.data
  (:require
   [thesis.parse :as parse]
   [clojure.string :as string]
   [clojure.java.io :as io])
  (:import
   [java.io BufferedReader FileReader File FilenameFilter]
   [edu.stanford.nlp.process DocumentPreprocessor]))

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

(defn clear-micusp-headers []
  "This takes the text version of the micusp papers (converted from the pdf by nu code, and strips out header text."
  (let [ptrn1 #"[\n\r][^A-Za-z]+[\n\r]" ;lines of only non alpha
	ptrn2 #"[\n\r]Michigan Corpus.*[\n\r]" ;lines beginning with...
	ptrn3 #"[\n\r]MICUSP.*[\n\r]"
	filename-filter (proxy [Object FilenameFilter] [] ;only open .txt files
			  (accept [dir name]
				  (boolean (re-find #".txt$" name))))
	dir-contents (.listFiles (File. *micusp-directory*) filename-filter)]
    (doseq [file dir-contents]
      (println (.toString file))
      (let [text (-> (slurp file)
		     (string/replace ptrn1 "")
		     (string/replace ptrn2 ""))]
	(spit file text)))))

(defn load-sentences [file-path]
  "Use Stanford Parser DocumentPreprocessor to returns a list of sentences for file-path. Sentences are lists of objects that implement HasWord"
  (seq (DocumentPreprocessor. file-path)))

(defn load-micusp-file [file-name]
  "file-name is the file name without filetype suffix"
  (load-sentences (str *micusp-directory* file-name ".txt")))


(def *es-samples* ["BIO.G1.01.1"
		   "BIO.G2.03.1"
		   "CEE.G1.02.2"
		   "CEE.G1.02.3"
		   "CEE.G1.02.1"
		   "PHI.G1.02.1"
		   "PHI.G1.02.2"
		   "POL.G2.01.1"])
(def *en-samples* ["BIO.G0.11.1"
		   "BIO.G1.05.1"
		   "CEE.G1.03.1"
		   "CEE.G3.04.2"
		   "CEE.G0.01.2"
		   "PHI.G3.03.1"
		   "PHI.G1.03.1"
		   "PHI.G0.06.5"
		   "POL.G0.15.1"])

(def *en-samples2* [])

(defn dump-micusp-parses-and-deps [& sample-lists]
  "dumps serialized java objects containing the parse trees and dependency graphs of the lists of micusp file names. Function intended to receive one or more lists of file names. resulting files have the same names with suffixes .parse and .deps"
  (doseq [fname (reduce into sample-lists)]
    (let [parse (-> fname
                    (load-micusp-file)
                    (parse/parse-sentences))
          deps (parse/dependencies parse)]
      (with-open [outp (-> (File. (str *micusp-directory* fname ".parse"))
                           java.io.FileOutputStream.
                           java.io.ObjectOutputStream.)]
        (.writeObject outp (to-array parse))
        (println "+"))
      (with-open [outp (-> (File. (str *micusp-directory* fname ".deps"))
                           java.io.FileOutputStream.
                           java.io.ObjectOutputStream.)]
        (.writeObject outp (to-array deps))
        (println "+")))))

(defn load-micusp-dump [name]
  (with-open [inp (-> (File. (str *micusp-directory* name))
                      java.io.FileInputStream.
                      java.io.ObjectInputStream.)]
    (seq (.readObject inp))))

(defn load-micusp-parse [name]
  "Loads a file that was previously dumped with dump-micusp-parses-and-deps. Arguments should not have suffix"
  (load-micusp-dump (str name ".parse")))

(defn load-micusp-deps [name]
  "Loads a file that was previously dumped with dump-micusp-parses-and-deps. Arguments should not have suffix"
  (load-micusp-dump (str name ".deps")))