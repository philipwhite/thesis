(ns thesis.arguments
  (:require
   [thesis.data :as data]
   [thesis.verb :as verb]
   [thesis.parse :as parse]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats]
   [clojure.string :as string])
  (:import
   edu.stanford.nlp.trees.EnglishGrammaticalStructure)
  (:use
   thesis.tools))

(def *all-argument-attributes*
  ["s" "S"
   "ao"  "aO" "Ao" "AO"
   "aio" "aiO" "aIo" "aIO" "Aio" "AIo" "AIO"
   "aoc" "aoC" "aOc" "aOC" "Aoc" "AOc" "AOC"
   "p" "P"
   "pc" "pC" "Pc" "PC"
   "sc" "sC" "Sc" "SC"])

(def *all-argument-attributes-by-structure*
  [["s" "S"]
   ["ao"  "aO" "Ao" "AO"]
   ["aio" "aiO" "aIo" "aIO" "Aio" "AIo" "AIO"]
   ["aoc" "aoC" "aOc" "aOC" "Aoc" "AOc" "AOC"]
   ["p" "P"]
   ["pc" "pC" "Pc" "PC"]
   ["sc" "sC" "Sc" "SC"]])

(def *all-argument-structures*
  ["s"
   "ao"
   "aio"
   "aoc"
   "p"
   "pc"
   "sc"])

(def *example-sentences*
  [["I walk" "s.png"]
   ["I hit him" "ao.png"]
   ["I gave John the book" "aio.png"]
   ["I called him a fool" "aoc.png"]
   ["I was hurt" "p.png"]
   ["I was appointed president" "pc.png"]])

(def *relns*
  ["nsubj" "nsubjpass" "xcomp" "dobj" "iobj" ])

(def *verb-tags* ["VB" "VBD" "VBG" "VBN" "VBP" "VBZ"])

(def *referential-forms*
  ["other" "another" "else" "same"
   "this" "that" "these" "those"
   "myself" "yourself" "herself" "himself" "itself" "oneself"
   "ourselves" "yourselves" "themselves"
   "mine" "yours" "hers" "his" "ours" "theirs"
   "me" "you" "her" "him" "it" "one" "us" "them"
   "I" "you" "she" "he" "we" "they"])
;;how about 'what' as in "what she said"

(defn word-eq [a b]
  (= (string/upper-case a) (string/upper-case b)))


(defn verb-tag-node? [node]
  "returns true if the value matches on of the items in *verb-tags*"
  (if (some #(= % (.value node)) *verb-tags*)
    true
    false))

(defn verb-word-node? [node]
  "return true if the parent has a value matching one of the items in *verb-tags*"
  (if-let [p (.parent node)]
    (verb-tag-node? p)
    false))

(defn dump-valency-deps [deps]
  "deps is a sequence of dependencies"
  (doseq [d deps]
    (if (verb-word-node? (.gov d))
      (println d))))

(defn tree-root [node]
  "returns the root of the node"
  (if-let [p (.parent node)]
    (recur p)
    node))



;;this map contains the dependencies that must be present with a
;;particular verb as the governor in order to match a particular argument
;;pattern. The keys are the names of the patterns, the values are lists
;;of relns. simpler structures are lower than the more complicated ones
;;that contain them
(def *structure-maps* ;TODO csubj and csubjpass
  {"pc" ["nsubjpass" "dobj"]
   "p" ["nsubjpass"]
   "aoc" ["nsubj" "xcomp"]
   "aio" ["nsubj" "iobj" "dobj"]
   "ao" ["nsubj" "dobj"]
   "s" ["nsubj"] })

(defn get-copula-arg-structure [deps verb-node]
  "gets to see if there is a 'cop' dependency among deps where the dependent
is verb node. Returns a similar value as get-argument-structure"
  (if-let [cop (->> deps
                 (filter #(= "cop" (.getShortName (.reln %))))
                 (filter #(= verb-node (.dep %)))
                 first)]
    ;;try to find a subject
    (if-let [subj (->> deps
                       (filter #(=any? ["nsubj" "csubj"]
                                       (.getShortName (.reln %))))
                       (filter #(= (.gov cop) (.gov %)))
                       first)]
      ["sc" (.dep subj) (.gov cop)])))

(defn get-argument-structure [deps verb-node]
  "return a seq where the first item is one of the all-lower case values in *all-argument-attributes* does not distinguish full NPs from anaphora. The remaining items are the arguments (as tree nodes)"
  (if-let [cop-arg-struct (get-copula-arg-structure deps verb-node)]
    cop-arg-struct
    ;;else, not a copula
    (let [deps-with-verb-gov
          (filter #(identical? (.gov %) verb-node) deps)
          reln-names (map #(.getShortName (.reln %)) deps-with-verb-gov)
          match (some (fn [[name pattern]]
                        (if (every? (fn [reln]
                                      (some #(= reln %) reln-names))
                                    pattern)
                          name))
                      *structure-maps*)]
      (when-not (nil? match)
        (let [arguments (for [reln (*structure-maps* match)]
                          (let [dep (some #(if (= reln (.getShortName (.reln %)))
                                             %)
                                          deps-with-verb-gov)]
                            (.dep dep)))]
          (if (= match "aoc")
            (if-let [reln-with-2nd-obj-gov
                     (some #(if
                                (and
                                 (= (.gov %) (second arguments))
                                 (= (.getShortName (.reln %)) "nsubj"))
                              %) deps)]
              [match (first arguments) (.dep reln-with-2nd-obj-gov)  (second arguments)])
            (into [match] arguments)))))))



(defn mark-referential-forms [[structure & arguments]]
  "pass this the output of get-argument-structure. It basically capitalized the
structure code unless the form is referential. It returns only the code"
  (apply str
         (let [pairs (map vector structure arguments)]
           (for [[code arg] pairs]
             (if (some #(word-eq (.value arg) %) *referential-forms*)
               code
               (string/upper-case code))))))

(defn extract-arguments-of-verb [deps verb-node]
  (let [structures (remove nil? (get-argument-structure deps verb-node))]
    (mark-referential-forms structures)))

(defn extract-arguments [deps]
  (let [parse (tree-root (.gov (first deps)))
        verb-nodes (filter verb-word-node? parse)]
    
    (remove empty?
            (map (partial extract-arguments-of-verb deps)
          verb-nodes))))