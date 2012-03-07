(ns thesis.parse
  (:import
    edu.stanford.nlp.parser.lexparser.LexicalizedParser
    edu.stanford.nlp.trees.LabeledScoredTreeNode
    edu.stanford.nlp.ling.StringLabel
    edu.stanford.nlp.parser.ui.TreeJPanel
    edu.stanford.nlp.trees.EnglishGrammaticalStructure
    com.chaoticity.dependensee.Main
    javax.swing.JFrame
    clojure.lang.ASeq
    java.awt.image.BufferedImage
    java.awt.Graphics
    javax.imageio.ImageIO
    java.io.File
    java.awt.Color
    com.lowagie.text.Document
    com.lowagie.text.pdf.PdfWriter
    java.io.FileOutputStream
    com.lowagie.text.pdf.PdfContentByte
    com.lowagie.text.pdf.PdfTemplate
    java.awt.Graphics2D)
  
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
  (try
    (do
      (Main/writeImage (.apply *parser* text) "latest-dep-image.png")
      (shell/sh "open" "latest-dep-image.png"))
    (catch Exception e nil)))

(defn write-deps-image [text title]
  (Main/writeImage (.apply *parser* text) title))

(comment (defn dependencies [text]
   (let [egs (EnglishGrammaticalStructure. (.apply *parser* text))]
     (dorun (map #(println (.toString %)) (.typedDependencies egs))))))

(defn deleafed-seq [s]
  "returns the seq s with the leaves (strings/words) removed"
  (if-not (string? (first s))
    (cons (first s) (remove nil? (map deleafed-seq (rest s))))))

(defn export-parse [text path width height]
  (let [p (.apply *parser* text)
        panel (TreeJPanel.)
        bi (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        g (.createGraphics bi)
        frame (JFrame. "Tree")]
    (.setTree panel p)
    (.setBackground panel Color/white)
    (doto frame
      (.setSize width height)
      (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
      (.setContentPane panel)
      ;(.pack)
      (.setVisible true))
    (.paint panel g)
    (.dispose g)
    (ImageIO/write bi "bmp" (File. path))
    ))
                                        ;(.dispose frame)

(comment (defn export-parse [text path width height]
   (let [doc (Document.)
         pdf-writer (PdfWriter/getInstance doc (FileOutputStream. path))
         p (.apply *parser* text)
         panel (TreeJPanel.)
         frame (JFrame. "Tree")]
     (.open doc)
     (let [cb (.getDirectContent pdf-writer)
           tp (.createTemplate cb width height)
           g2 (.createGraphics tp width height)]
       (.setTree panel p)
       (.setBackground panel Color/white)
       (doto frame
         (.setSize 640 480)
         (.setDefaultCloseOperation JFrame/DISPOSE_ON_CLOSE)
         (.setContentPane panel)
         (.setVisible true))
       (.print panel g2)
       (.dispose g2)
       (.addTemplate cb tp 0 0)
       (.dispose frame)
       (.close doc)))))


(defn parse->dot-2 [node root trim-punct]
  (if (and trim-punct (some #(= (.value node) %) ["," "." ";" ":" "!" "?"]))
    ""
    (str
     (str "N" (.nodeNumber node root) " [label=\"" (.value node) "\" shape=plaintext fontsize=20]\n")
     (if-let [parent (.parent node root)]
       (str "N" (.nodeNumber parent root) "--N" (.nodeNumber node root) "\n")))))

(defn parse->dot [text path trim-punct]
  "writes a dot file of the parse of the string in text"
  (let [parse (if (string? text)
                (.apply *parser* text)
                text)]
    (spit path
     (apply str
            (concat ["graph Tree {\nsplines=false\nmargin=0\n"]
                    (map #(parse->dot-2 % (.firstChild parse) trim-punct) (.firstChild parse))
                    ["}"])))))