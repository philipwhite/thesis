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

(def *wricle-directory* "../data/WricleCorpusv13/Files/")

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

(defn load-wricle-file [file-name]
  "file-name is the file name without filetype suffix"
  (load-sentences (str *wricle-directory* file-name ".txt")))

(defn dump-parses-and-deps [corpus & sample-lists]
   "dumps serialized java objects containing the parse trees and dependency graphs of the lists of file names. Function intended to receive one or more lists of file names. resulting files have the same names with suffixes .parse and .deps. First argument should be either :wricle or :micusp"
   (let [[load-file dir] (case corpus
                           :micusp [load-micusp-file *micusp-directory*]
                           :wricle [load-wricle-file *wricle-directory*]
                           (throw
                            (Exception. "First argument must indicate corpus")))]
     (doseq [fname (reduce into sample-lists)]
       (let [parse (-> fname
                       load-file
                       parse/parse-sentences)
             deps (parse/dependencies parse)]
         (with-open [outp (-> (File. (str dir fname ".parse"))
                              java.io.FileOutputStream.
                              java.io.ObjectOutputStream.)]
           (.writeObject outp (to-array parse))
           (println "+"))
         (with-open [outp (-> (File. (str dir fname ".deps"))
                              java.io.FileOutputStream.
                              java.io.ObjectOutputStream.)]
           (.writeObject outp (to-array deps))
           (println "-"))))))


(defn load-dump [corpus name]
   (let [dir (case corpus
               :micusp *micusp-directory*
               :wricle *wricle-directory*
               (throw
                (Exception. "First argument must indicate corpus")))]
     (with-open [inp (-> (File. (str dir name))
                         java.io.FileInputStream.
                         java.io.ObjectInputStream.)]
       (seq (.readObject inp)))))

 (defn load-parse [corpus name]
   "Loads a file that was previously dumped with dump-parses-and-deps. Arguments should not have suffix"
   (load-dump corpus (str name ".parse")))

 (defn load-deps [corpus name]
   "Loads a file that was previously dumped with dump-parses-and-deps. Arguments should not have suffix"
   (load-dump corpus (str name ".deps")))

 (def *micusp-es* ["BIO.G1.01.1"
                   "BIO.G2.03.1"
                   "CEE.G1.02.2"
                   "CEE.G1.02.3"
                   "CEE.G1.02.1"
                   "PHI.G1.02.1"
                   "PHI.G1.02.2"
                   "POL.G2.01.1"])

(def *micusp-en*
  ["BIO.G0.11.1"
   "BIO.G1.05.1"
   "CEE.G1.03.1"
   "CEE.G3.04.2"
   "CEE.G0.01.2"
   "PHI.G3.03.1"
   "PHI.G1.03.1"
   "PHI.G0.06.5"
   "POL.G0.15.1"
   "LIN.G0.01.1"
   "LIN.G0.01.2"
   "MEC.G0.02.1"
   "MEC.G0.03.1"
   "NRE.G0.01.1"
   "NRE.G0.02.1"
   "NUR.G0.01.1"
   "NUR.G0.02.1"
   
   "BIO.G1.04.1"
   "BIO.G3.03.1"
   "BIO.G3.02.1"
   "BIO.G0.01.1"
   "BIO.G0.02.1"
   "BIO.G0.02.2"
   "BIO.G0.02.3"
   "BIO.G0.02.4"
   "BIO.G0.02.5"
   "BIO.G0.04.1"
   "BIO.G0.05.1"
   "BIO.G0.06.1"
   "BIO.G0.07.1"
   "BIO.G0.09.1"
   "BIO.G0.11.3"
   "BIO.G0.12.2"
   "BIO.G0.13.1"
   "BIO.G0.16.1"
   "BIO.G0.18.1"
   "BIO.G0.19.1"
   "BIO.G0.20.1"
   "BIO.G0.24.1"
   "BIO.G0.25.1"
   "BIO.G0.26.1"
   "BIO.G0.29.1"
   "BIO.G0.30.1"
   "BIO.G0.32.1"
   "BIO.G1.03.1"
   "BIO.G1.06.1"
   "BIO.G2.02.1"
   "BIO.G2.07.1"])
; "PSY.G0.01.1"
;   "PSY.G0.02.1"
(def *wricle-es* ["A99-1"
                  "A99-2"
                  "A255-1"
                  "A255-2"
                  "A364-1"
                  "A364-2"
                  "C21-1"
                  "C21-3"
                  "A43-1"
                  "A43-2"
                  "A55-1"
                  "A55-2"
                  "A91-1"
                  "A91-2"
                  "A144-1"
                  "A144-2"
                  "A148-1"
                  "A148-2"
                  "A170-1"
                  "A170-2"
                  "A240-1"
                  "A243-1"
                  "A243-2"
                  "A249-1"
                  "A249-2"
                  "A267-1"
                  "A267-2"
                  "A277-1"
                  "A277-2"
                  "A356-1"
                  "A357-1"
                  "A357-2"
                  "C2-1"
                  "C2-2"
                  "C2-3"
                  "C12-2"
                  "C12-3"
                  "C15-1"
                  "C15-2"
                  "C15-3"
                  "C19-1"
                  "C19-2"
                  "C19-3"
                  "C22-1"
                  "C22-2"
                  "C22-3"
                  "C24-1"
                  "C24-2"
                  "C24-3"
                  "C28-1"
                  "C28-3"
                  "C35-1"
                  "C35-3"
                  "C46-1"
                  "C46-2"
                  "C46-3"
                  "C50-1"
                  "C50-2"
                  "C50-3"
                  "C57-1"
                  "C63-1"
                  "C63-2"
                  "C63-3"
                  "C70-2"
                  "C70-3"
                  "C73-1"
                  "C73-2"
                  "C73-3"
                  "C75-1"
                  "C75-3"
                  "C76-1"
                  "C76-2"
                  "C76-3"
                  "C107-1"
                  "C107-2"
                  "C107-3"
                  "C118-1"
                  "C118-2"
                  "C118-3"])

