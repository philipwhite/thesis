(ns thesis.parse
  (:import
    edu.stanford.nlp.parser.lexparser.LexicalizedParser
    edu.stanford.nlp.trees.LabeledScoredTreeNode
    edu.stanford.nlp.ling.StringLabel
    edu.stanford.nlp.parser.ui.TreeJPanel
    edu.stanford.nlp.trees.EnglishGrammaticalStructure
    com.chaoticity.dependensee.Main
    javax.swing.JFrame
    clojure.lang.ASeq)
  (:require
    [clojure.string :as str]
    [clojure.contrib.string :as cstr]
    [clojure.xml :as xml]
    [clojure.java.shell :as shell]))
(defmacro brk [& more] `(debug-repl ~@more))

(def *parser-path* "../stanford/grammar/englishPCFG.ser.gz")
;(def *parser-path* "../stanford/grammar/englishFactored.ser.gz")
(def *parser* (LexicalizedParser. *parser-path*))
(.setOptionFlags *parser* (into-array ["-retainTmpSubcategories"]))

(defn pick-sentence [s]
  "s must be a parse tree (LabeledScoredTreeNode). returns s if the root is S, nil otherwise. In otherwords, returns nil for nonsentences"
  (if (= "S" (.value (first (.children s))))
    s))

(defn parse-sentences [ss]
  "parses the sentences in ss (sequence of lists of objects that implement HasWord), removing any that do not have an S as the ROOT. Returns a sequence of trees."
  (remove nil? (map pick-sentence (map #(.apply *parser* %) ss))))

(defn dependencies [ps]
  "ps == parsed sentences. This function works on a sequence of sentences. Returns a sequence of dependency listings."
  (map #(.typedDependencies (EnglishGrammaticalStructure. %)) ps))

(def *all-deps*
     (vec (.keySet (EnglishGrammaticalStructure/shortNameToGRel))))

(defn tree->seq [t]
  (with-meta (cons (if (.isLeaf t)
                     (.value t)
                     (keyword (.value t)))
               (map tree->seq (.children t)))
    {:labeled-score-tree t}))

(defn text->seq [text]
  (tree->seq (.apply *parser* text)))

(defn seq->tree [s]
  "returns a tree (likely LabeledScoredTreeNode) using the metadata. nil if metadata missing"
  (:labeled-score-tree (meta s)))

(defn has-tree-meta? [x]
  (contains? (meta x) :labeled-score-tree))

(defn reconstruct [s]
  (str/join " " (filter string? (flatten s))))

(defn symbol->label [symb]
  (cstr/drop 1 (str symb)))

(defn reconstruct-tree [d]
  "Returns an object of type LabeledScoredTreeNode. Takes a deleafed sequence"
  (LabeledScoredTreeNode. 
    (StringLabel. (symbol->label (first d)))
    (map reconstruct-tree (rest d))))

(defn display-tree [t]
  (let [tree (cond
               (has-tree-meta? t) (seq->tree t)
               (seq? t) (reconstruct-tree t)
               :else t)
        frame (JFrame. "Tree")
        panel (TreeJPanel.)]
    (.setTree panel tree)
    (doto frame
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setContentPane panel)
      (.pack)
      (.setVisible true))))

(defn display-parse [text]
  (display-tree (.apply *parser* text)))

(defn display-deps [text]
  (Main/writeImage (.apply *parser* text) "latest-dep-image.png")
  (shell/sh "open" "latest-dep-image.png"))

(defn write-deps-image [text title]
  (Main/writeImage (.apply *parser* text) title))

(comment (defn dependencies [text]
   (let [egs (EnglishGrammaticalStructure. (.apply *parser* text))]
     (dorun (map #(println (.toString %)) (.typedDependencies egs))))))

(defn deleafed-seq [s]
  "returns the seq s with the leaves (strings/words) removed"
  (if-not (string? (first s))
    (cons (first s) (remove nil? (map deleafed-seq (rest s))))))







