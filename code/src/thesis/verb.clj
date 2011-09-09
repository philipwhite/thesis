(ns thesis.verb
  (:import
    edu.stanford.nlp.parser.lexparser.LexicalizedParser
    edu.stanford.nlp.trees.LabeledScoredTreeNode
    edu.stanford.nlp.ling.StringLabel
    edu.stanford.nlp.trees.EnglishGrammaticalStructure
    edu.stanford.nlp.util.Filter)
  (:require
   [thesis.parse :as parse]
   [clojure.string :as string]
   [clojure.contrib.seq-utils :as sq]))

(comment (defn test [text]
   (extract-VPs (.apply parse/*parser* text))))

(defn extract-verbs [parse]
  "Takes a parse of a sentence, looks for finite verbs, and returns a seq of maps. maps have keys->values :verb -> string, :progressive? -> bool, :past? -> bool, :perfect? -> bool, :passive? -> bool. "
  nil)

(defn extract-VP-VP [parse]
  "Helper for extract-VPs, expects a parse headed by VP"
  (vector parse))

(defn extract-VP-S [parse]
  "Helper for extract-VPs, expects a tree headed by S"
  ;;there should either be multiple S children or a single VP child
  ;;perform error checking here to ensure that is the case
  (let [children (.children parse)
        vp-child-count (apply + (map #(if (= (.value %) "VP") 1 0) children))
        s-child-count (apply + (map #(if (= (.value %) "S") 1 0)  children))]
    ;;error checking
    (if (not (or
              (and (== 1 vp-child-count) (== 0 s-child-count))
              (and (== 0 vp-child-count) (> s-child-count 1))))
      (throw (Exception. (str "extract-VP-S unexpected S child count. num of VP =" vp-child-count " num of S = " s-child-count))))
    (if (== vp-child-count 1)
      (extract-VP-VP (first (filter #(= "VP" (.value %)) children)))
      (map extract-VP-S (filter #(= "S" (.value %)) children)))))

(defn extract-VPs [parse]
  "returns a seq of subtrees of largely finite predicates"

  ;;from ROOT, follow S or return nil if S not present
  ;;from S, follow each S or the VP. For each S repeat this line
  ;;for the VP, if there is more than one child VP apply this line to those
  ;;if there is 1 or 0 child VP, return the current VP
  (if (.isLeaf parse)
    nil
    (do
      ;;make sure that there is an expect number of children
      (let [ch-count (count (.children parse))]
        (if (or (== 0 ch-count) (> ch-count 1))
          (throw (Exception. (str "extract-VPs unexpected ROOT child count = " ch-count)))))
;;at this point we know there is a single child
      (let [child (first (.children parse))]
        (if (= (.value child)
             "S")
          (flatten (extract-VP-S child))
          nil)))))

;;;try 2
;;;

(defn children-of-type [parent type]
  (filter #(= (string/capitalize type)
              (string/capitalize (.value %)))
          (.children parent)))

(defn has-descendants? [parent path]
  "returns true if the sequence of strings in 'path' can be matched with a series of descendants
   of those values with the immediate child of the parent being the leftmost in the sequence."
  (let [matches (children-of-type parent (first path))]
    (cond
     (empty? matches) false
     (empty? (rest path)) true
     :else (true? (some #(has-descendants? % (rest path))
                        matches)))))

(comment
  (defn mine-VP [vp]
   (let [modals (children-of-type vp "MD")]
     (if (empty? modals)
       (mine-VP-nonmodal vp)
       (map (children-of-type vp "VP")
            #(mine-VP-modal % modals)))))

 (defn mine-VP-modal [vp modals]
   (if (has-descendants? vp ["VB" "have"])
     (map (children-of-type vp "VP")
          #(mine-VP-modal-perf % modals))
     (mine-VP-modal-imperf vp modals)))

 (defn mine-VP-nonmodal [vp]
   )

 (defn mine-VP-modal-perf [vp modals]
   (if (and (has-descendants? vp ["VBN" "been"])
            (has-descendants? vp ["VP" "VBG"]))
     (map (children-of-type vp "VP")
          #(mine-VP-modal-perf-pass % modals))
     (mine-VP-modal-perf-active vp modals)))

 (defn mine-VP-modal-imperf [vp modals]
   )

 (defn mine-VP-modal-perf-pass [vp modals]
   ))



;;;
;;;PRESENT

;modal perfect progressive
["VP"
 ["MD"]
 ["VP"
  ["VB"
   ["have"]]
  ["VP"
   ["VBN"
    ["been"]]
   ["VP"
    ["VBG"]]]]]

;;modal progressive
["VP"
 ["MD"]
 ["VP"
  ["VB"
   ["be"]]
  ["VP"
   ["VBG"]]]]

;;modal perfect
["VP"
 ["MD"]
 ["VP"
  ["VB"
   ["have"]]
  ["VP"
   ["VBN"]]]]

;;modal simple
["VP"
 ["MD"]
 ["VP"
  ["VB"]]]


;;;
;;;PAST

;;past perfect progressive
["VP"
 ["VBD"
  ["had"]]
 ["VP"
  ["VBN"
   ["been"]]
  ["VP"
   ["VBG"
    ["writing"]]]]]

;;past progressive
["VP"
 ["VBD"
  ["was"]]
 ["VP"
  ["VBG"]]]

["VP"
 ["VBD"
  ["were"]]
 ["VP"
  ["VBG"]]]

;;past perfect
["VP"
 ["VBD"
  ["had"]]
 ["VP"
  ["VBN"]]]

;;past simple
["VP"
 ["VBD"]]

;;;
;;;PRESENT

;;present perfect progressive
["VP"
 ["VBZ"
  ["has"]]
 ["VP"
  ["VBN"
   ["been"]]
  ["VP"
   ["VBG"]]]]

["VP"
 ["VBP"
  ["have"]]
 ["VP"
  ["VBN"
   ["been"]]
  ["VP"
   ["VBG"]]]]

;;present progressive
["VP"
 ["VBZ"
  ["is"]]
 ["VP"
  ["VBG"]]]

["VP"
 ["VBP"
  ["am"]]
 ["VP"
  ["VBG"]]]

["VP"
 ["VBP"
  ["are"]]
 ["VP"
  ["VBG"]]]

;;present perfect
["VP"
 ["VBZ"
  ["has"]]
 ["VP"
  ["VBN"]]]

["VP"
 ["VBP"
  ["have"]]
 ["VP"
  ["VBN"]]]

;;present simple
["VP"
 ["VBZ"]]

["VP"
 ["VBP"]]

(defn tree->seq [t]
  (with-meta (cons (.value t)
               (map tree->seq (.children t)))
    {:labeled-score-tree t}))

(defn text->seq [text]
  (tree->seq (.apply parse/*parser* text)))


(defn split-VP-conjunctions-1 [head node]
  ;;first determine if this node's children contains a splittable
  ;;conjunction
  (let [VP-children (children-of-type node "VBP")]
    (if (and (has-descendants? node ["CC"])
             (> (count VP-children) 1))
      ;;process each VP child ONE BY ONE (not in parallel!)
      (loop [heads []
             rest-children VP-children]
        (if-let [child (first rest-children)]
          (let [other-children (filter #(= child %) VP-children)
                other-children (into other-children
                                     (children-of-type node "CC"))
                child-filter (proxy [edu.stanford.nlp.util.Filter] []
                               (accept [node]
                                 (if (some #(= node %) other-children)
                                   false
                                   true)))
                new-head (.prune head child-filter)]
            (recur (conj heads new-head) (rest rest-children)))
          heads))
      ;;else just return the head in a sequence
      (vector head))))

(defn split-VP-conjunctions [parse]
  (split-VP-conjunctions-1 parse parse))