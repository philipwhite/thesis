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
(def *misc-directory* "../data/misc/")
(def *sulec-directory* "../data/sulec/")
(def *sulec-raw-file* (str *sulec-directory* "sulec-all-raw.txt"))
(def *ice-can-directory* "../data/ice-canada/processed/")
(def *ice-can-raw-directory* "../data/ice-canada/Corpus/")
(def *ice-hk-directory* "../data/ice-hk/processed/")
(def *ice-hk-raw-directory* "../data/ice-hk/CORPUS/")
(def *brown-b-directory* "../data/brown/processed/")
(def *brown-b-raw-directory* "../data/brown/raw/")

(def keyword-to-directory
  {:micusp *micusp-directory*
   :wricle *wricle-directory*
   :misc *misc-directory*
   :sulec *sulec-directory*
   :ice-can *ice-can-directory*
   :ice-hk *ice-hk-directory*
   :brown-b *brown-b-directory*})

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



(defn divide-sulec []
  "Take the sulec corpus and divide it into component essays"
  (let [parts (string/split (slurp *sulec-raw-file*)
                            #"[\n\r][\n\r][\n\r]")]
    (loop [remaining-parts parts
           name 0]
      (if remaining-parts
        (do
          (spit (str *sulec-directory* name ".txt")
                (reduce #(str %1 "\n" %2) (take 6 remaining-parts)))
          (recur (nthnext remaining-parts 6)
                (+ name 1)))))))

(defn load-sentences [file-path]
  "Use Stanford Parser DocumentPreprocessor to returns a list of sentences for file-path. Sentences are lists of objects that implement HasWord"
  (seq (DocumentPreprocessor. file-path)))


(defn corpus-file-path [corpus file-name]
  (str (keyword-to-directory corpus)
                       file-name
                       ".txt"))

(defn load-corpus-file [corpus file-name]
  (load-sentences (str (keyword-to-directory corpus)
                       file-name
                       ".txt")))


(defn dump-stats [corpus & sample-lists]
  "writes a text file for each input file with a single number that is the number of words in that file"
  (let [dir (if (contains? keyword-to-directory corpus)
               (keyword-to-directory corpus)
               (throw
                (Exception. "First argument must indicate corpus")))]
     (doseq [fname (reduce into sample-lists)]
       (let [doc (load-corpus-file corpus fname)
             count (apply + (map #(count %) doc))]
         (spit (str dir fname ".stats")
               (str count))))))

(defn dump-parses-and-deps [corpus & sample-lists]
   "dumps serialized java objects containing the parse trees and dependency graphs of the lists of file names. Function intended to receive one or more lists of file names. resulting files have the same names with suffixes .parse and .deps. First argument should be either :wricle or :micusp"
   (let [dir (if (contains? keyword-to-directory corpus)
               (keyword-to-directory corpus)
               (throw
                (Exception. "First argument must indicate corpus")))]
     (doseq [fname (reduce into sample-lists)]
       (let [parse (->> fname
                        (load-corpus-file corpus)
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
  (if-let [dir (keyword-to-directory corpus)]
    (with-open [inp (-> (File. (str dir name))
                        java.io.FileInputStream.
                        java.io.ObjectInputStream.)]
      (seq (.readObject inp)))
    (throw
     (Exception. "First argument must indicate corpus"))))

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
(def *wricle-es-uncombined* ["A99-1"
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

(defn combine-wricle []
  "take the files in *wricle-es-uncombined* and pair them up into single files"
  (let [fpaths (map #(str *wricle-directory*  % ".txt") *wricle-es-uncombined*)]
    ;;there are an odd number of wricle-es files
    ;;take the last one and add it to the last pair
    (loop [rest-paths fpaths
           iter 0]
      (if (= (count rest-paths) 3)
        (spit (str *wricle-directory* iter ".txt")
              (str (slurp (nth rest-paths 0)) "\n"
                   (slurp (nth rest-paths 1)) "\n"
                   (slurp (nth rest-paths 2))))
        (do
          (spit (str *wricle-directory* iter ".txt")
                (str (slurp (nth rest-paths 0)) "\n"
                     (slurp (nth rest-paths 1)) ))
          (recur (nthnext rest-paths 2) (+ iter 1)))))))

(def *wricle-es* (map str (range 39)))

(def *sulec-es* (map str (range 21)))

(def *ice-can-en*
  ["W1A-001"
   "W1A-003"
   "W1A-005"
   "W1A-006"
   "W1A-010"
   "W1A-013"
   "W1A-014"
   "W1A-016"
   "W1A-017"
   "W1A-020"
   "W1B-016"
   "W1B-019"
   "W1B-020"])

(def *ice-can-es*
  ["W1A-002"])

(def *ice-hk-en*
  ["W1B-029"
   "W1B-029"
   "W1B-030"
   "W2A-037"
   "W2B-002"
   "W2B-008"
   "W2B-021"
   "W2B-025"
   "W2B-032"
   "W2B-034"
   "W2B-037"
   "W2B-039"
   "W2B-040"
   "W2D-011"
   "W2D-015"
   "W2D-016"
   "W2D-018"
   "W2D-019"
   "W2D-020"
   "W2F-012"
   "W2F-013"
   "W2F-015"
   "W2F-018"
   "W2F-019"])

(def *brown-b-en*
  (into (map (partial str "cb" "0") (range 1 10)) (map (partial str "cb") (range 10 28))))

(def *all-corpora*
  [{:corpus :micusp
    :filenames *micusp-es*
    :L1 :es}
   {:corpus :micusp
    :filenames *micusp-en*
    :L1 :en}
   {:corpus :wricle
    :filenames *wricle-es*
    :L1 :es}
   {:corpus :misc
    :filenames ["msu-level4"]
    :L1 :es}
   {:corpus :ice-can
    :filenames *ice-can-en*
    :L1 :en}
   {:corpus :ice-can
    :filenames *ice-can-es*
    :L1 :es}
   {:corpus :sulec
    :filenames *sulec-es*
    :L1 :es}
   {:corpus :ice-hk
    :filenames *ice-hk-en*
    :L1 :en}
   {:corpus :brown-b
   :filenames *brown-b-en*
   :L1 :en}]
  )

(def *es-corpora*
  (filter #(= (:L1 %) :es) *all-corpora*))

(def *en-corpora*
  (filter #(= (:L1 %) :en) *all-corpora*))

(defn cleanup-ice-can []
  "remove tags <...> from the texts"
  (let [files (concat *ice-can-es* *ice-can-en*)]
    (doseq [f files]
      (let [fpath (str *ice-can-directory* f ".txt")
            text (slurp fpath)]
        (spit fpath
              (-> text
                  
                  (string/replace #"\<.+\>" "")
                 
                  (string/replace #"[\r\n][\r\n]" " ")))))))

(defn cleanup-ice [file-names path-in path-out]
  "remove all tags and some text between tags"
  (doseq [f file-names]
    (let [text (slurp (str path-in f ".txt"))]
      (spit (str path-out f ".txt")
            (-> text
                (string/replace #"&dollar;" "")
                (string/replace #"&ldquo;|&rdquo;" "\"")
                (string/replace #"&obrack;" "[")
                (string/replace #"&cbrack;" "]")
                (string/replace #"\<O\> deleted \</O\>" "Marcus") ;;redacted names
                (string/replace #"\<O\>.*\</O\>" "") ;;other extra-corporal text
                (string/replace #"\<-\>.*\</-\>" "") ;;misspelled words (the corr'n is adjacent)
                (string/replace #"\<X\>.*\</X\>" "") ;;extra-corpus text
                (string/replace #"\<&\>.*\</&\>" "") ;comments
                (string/replace #"\<.+\>" ""))))))

(defn strip-brown-tags []
  "removes the tags from brown. In the raw format tokens are of the form word/tag"
  (doseq [f *brown-b-en*]
    (let [text (slurp (str *brown-b-raw-directory* f))]
      (spit (str *brown-b-directory* f ".txt")
            (string/replace text #"(?<=\S)/(\S*)" "")))))

;;for testing purposes, randomly assign the corpora instances to two different
;;language groupings
(let [rand-corpora
      (for [c *all-corpora*
	    f (:filenames c)]
	(into c {:filenames [f] :L1 (if (> 0.5 (rand)) :es :en)}))]
  (def *rand-es-corpora* (filter #(= (:L1 %) :es) rand-corpora))
  (def *rand-en-corpora* (filter #(= (:L1 %) :en) rand-corpora)))

(defn count-words-in-file [file]
  (let [f (slurp file)
        lines (string/split-lines f)
        words (mapcat #(string/split % #" ") lines)
        words (remove #(= "" %) words)]
    (count words)))

(defn count-words-in-corpora []
  (doseq [corp *all-corpora*]
    (print (corp :corpus) " " (corp :L1)
           (apply + (map count-words-in-file
                         (map (partial corpus-file-path (corp :corpus))
                              (corp :filenames))))
           "\n\n")))

(defn count-files-in-corpora []
  {:es (apply + (map #(count (:filenames %)) (filter #(= :es (:L1 %)) *all-corpora*)))
   :en (apply + (map #(count (:filenames %)) (filter #(= :en (:L1 %)) *all-corpora*)))})

(defn count-parsed-tokens-in-file [corpus file]
  (let [parse (load-parse corpus file)]
    (apply + (map #(count (.taggedYield %)) parse))))

(defn count-parsed-tokens-in-corpora []
  (doseq [corp *all-corpora*]
    (print (corp :corpus) " " (corp :L1)
           (apply + (map (partial count-parsed-tokens-in-file (corp :corpus))
                         (corp :filenames)))
           "\n\n")))

(defn count-tag-in-file [tag corpus file]
  (let [parse (load-parse corpus file)]
    (apply + (map #(count (filter (fn [tw] (= (.tag tw) tag)) (.taggedYield %))) parse))))

(defn count-tag-in-corpora [tag]
  (doseq [corp *all-corpora*]
    (print (corp :corpus) " " (corp :L1)
           (apply + (map (partial count-tag-in-file tag (corp :corpus))
                         (corp :filenames)))
           "\n\n")))

(defn print-deps-in-corpus [corpus files reln]
  (doseq [f files]
    (doseq [dep (mapcat seq (load-deps corpus f))]
      (if (= (.getShortName (.reln dep)) reln)
        (println f "\n" (.value (.dep dep)) (.value (.gov dep)))))))