(ns thesis.verb
  (:import
    edu.stanford.nlp.parser.lexparser.LexicalizedParser
    edu.stanford.nlp.trees.LabeledScoredTreeNode
    edu.stanford.nlp.ling.StringLabel
    edu.stanford.nlp.trees.EnglishGrammaticalStructure
    edu.stanford.nlp.util.Filter
    java.util.Vector)
  (:require
   [thesis.parse :as parse]
   [clojure.string :as string]
   [clojure.contrib.seq-utils :as sq]))

(comment (defn test [text]
   (split-conjunctions (.apply parse/*parser* text))
   ))

(comment (defn extract-verbs [parse]
   "Takes a parse of a sentence, looks for finite verbs, and returns a seq of maps. maps have keys->values :verb -> seq of strings, :modals -> seq of strings, :progressive? -> bool, :past? -> bool, :perfect? -> bool, :passive? -> bool. "
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

)

(defn children-of-type [parent type]
   (filter #(= (string/capitalize type)
               (string/capitalize (.value %)))
           (.children parent)))

(defn has-child-of-type? [parent child-type]
   "returns true if the parent has one or more children of type child-type"
   (some #(= % child-type) (map #(.value %) (.children parent))))

 (defn has-descendants? [parent path]
   "returns true if the sequence of strings in 'path' can be matched with a series of descendants
   of those values with the immediate child of the parent being the leftmost in the sequence."
   (let [matches (children-of-type parent (first path))]
     (cond
      (empty? matches) false
      (empty? (rest path)) true
      :else (true? (some #(has-descendants? % (rest path))
                         matches)))))

(defn get-tense-aspect-name [p]
  (let [subtitles {:past?  ["-past" "-present"]
                   :perfect? ["-perfect" ""]
                   :progressive? ["-progressive" ""]
                   :passive? ["-passive" ""]}
        modal-subtitle (if (not (zero? (count (:modal p)))) "-modal" "")]
    (let [ss (merge-with #(if %2 (first %1) (second %1))
                         subtitles p)]
      (string/replace-first
       (str modal-subtitle (:past? ss) (:perfect? ss) (:progressive? ss) (:passive? ss))
       #"-"
       ""))))

(def *tense-aspect-count* 24)

(def *tense-examples*
  ["He writes"
   "He wrote"
   "He will write"
   "He has written"
   "He had written"
   "He will have written"
   "He is writing"
   "He was writing"
   "He will be writing"
   "He has been writing"
   "He had been writing"
   "He will have been writing"])

(def *passive-examples*
  ["He is chased"
   "He was chased"
   "He will be chased"
   "He has been chased"
   "He had been chased"
   "He will have been chased"
   "He is being chased"
   "He was being chased"
   "He will be being chased"
   "He has been being chase"
   "He had been being chased"
   "He will have been being chased"])

(defn draw-tenses []
  (map parse/display-parse
       *tense-examples*))

(def *verb-patterns*
  (let [f (fn [past perfect progressive passive]
            {:past? past :perfect? perfect :progressive? progressive
             :passive? passive})
        Y true
        N false
        unsorted-patterns
        ;;;modal perfect progressive
        ;;active
        [{:pattern                             
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
          :depth 5
          :form (f N Y Y N)},

         ;;passive
         {:pattern                             
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"
              ["been"]]
             ["VP"
              ["VBG"
               ["being"]]
              ["VP"
                ["VBN"]]]]]]
          :depth 6
          :form (f N Y Y Y)}
         
         {:pattern                             
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"
              ["been"]]
             ["VP"
              ["VBG"
               ["getting"]]
              ["VP"
                ["VBN"]]]]]]
          :depth 6
          :form (f N Y Y Y)}
         
         ;;modal progressive
         ;;active
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["be"]]
            ["VP"
             ["VBG"]]]]
          :depth 4
          :form (f N N Y N)},

         ;;passive
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["be"]]
            ["VP"
             ["VBG"
              ["being"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N N Y Y)}
         
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["be"]]
            ["VP"
             ["VBG"
               ["getting"]]
              ["VP"
                ["VBN"]]]]]
          :depth 5
          :form (f N N Y Y)}

         ;;modal perfect
         ;;active
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N N)}

         ;;passive
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"
              ["been"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"
              ["gotten"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["have"]]
            ["VP"
             ["VBN"
              ["got"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y N Y)}

         ;;modal simple
         ;;active
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"]]]
          :depth 3
          :form (f N N N N)},

         ;;passive
         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["be"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N N Y)}

         {:pattern
          ["VP"
           ["MD"]
           ["VP"
            ["VB"
             ["get"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N N Y)}
;;;
;;;PAST

         ;;past perfect progressive
         ;;active
         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"]]]]
          :depth 4
          :form (f Y Y Y N)}

         ;;passive
         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["being"]]
             ["VP"
              ["VBD"]]]]]
          :depth 5
          :form (f Y Y Y Y)},

         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["getting"]]
             ["VP"
              ["VBD"]]]]]
          :depth 5
          :form (f Y Y Y Y)}
         
         ;;past progressive
         ;;active
         {:pattern
          ["VP"
           ["VBD"
            ["was"]]
           ["VP"
            ["VBG"]]]
          :depth 3
          :form (f Y N Y N)},

         
         {:pattern
          ["VP"
           ["VBD"
            ["were"]]
           ["VP"
            ["VBG"]]]
          :depth 3
          :form (f Y N Y N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBD"
            ["was"]]
           ["VP"
            ["VBG"
             ["being"]]
            ["VP"
             ["VBD"]]]]
          :depth 4
          :form (f Y N Y Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["were"]]
           ["VP"
            ["VBG"
             ["being"]]
            ["VP"
             ["VBD"]]]]
          :depth 4
          :form (f Y N Y Y)}
         
         {:pattern
          ["VP"
           ["VBD"
            ["was"]]
           ["VP"
            ["VBG"
             ["getting"]]
            ["VP"
             ["VBD"]]]]
          :depth 4
          :form (f Y N Y Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["were"]]
           ["VP"
            ["VBG"
             ["getting"]]
            ["VP"
             ["VBD"]]]]
          :depth 4
          :form (f Y N Y Y)}
         
         ;;past perfect
         ;;active
         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f Y Y N N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f Y Y N Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["gotten"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f Y Y N Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["had"]]
           ["VP"
            ["VBN"
             ["got"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f Y Y N Y)}

         ;;past simple
         ;;active
         {:pattern
          ["VP"
           ["VBD"]]
          :depth 2
          :form (f Y N N N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBD"
            ["was"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f Y N N Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["were"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f Y N N Y)}

         {:pattern
          ["VP"
           ["VBD"
            ["got"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f Y N N Y)}
;;;
;;;PRESENT

         ;;present perfect progressive
         ;;active
         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"]]]]
          :depth 4
          :form (f N Y Y N)},

         {:pattern
          ["VP"
           ["VBP"
            ["have"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"]]]]
          :depth 4
          :form (f N Y Y N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["being"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y Y Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["have"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["being"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y Y Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["getting"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y Y Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["have"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBG"
              ["getting"]]
             ["VP"
              ["VBN"]]]]]
          :depth 5
          :form (f N Y Y Y)}

         ;;present progressive
         ;;active
         {:pattern
          ["VP"
           ["VBZ"
            ["is"]]
           ["VP"
            ["VBG"]]]
          :depth 3
          :form (f N N Y N)},

         {:pattern
          ["VP"
           ["VBP"
            ["am"]]
           ["VP"
            ["VBG"]]]
          :depth 3
          :form (f N N Y N)}

         {:pattern
          ["VP"
           ["VBP"
            ["are"]]
           ["VP"
            ["VBG"]]]
          :depth 3
          :form (f N N Y N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBZ"
            ["is"]]
           ["VP"
            ["VBG"
             ["being"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["am"]]
           ["VP"
            ["VBG"
             ["being"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["are"]]
           ["VP"
            ["VBG"
             ["being"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["is"]]
           ["VP"
            ["VBG"
             ["getting"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["am"]]
           ["VP"
            ["VBG"
             ["getting"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["are"]]
           ["VP"
            ["VBG"
             ["getting"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N N Y Y)}
         
         ;;present perfect
         ;;active
         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N Y N N)},

         {:pattern
          ["VP"
           ["VBP"
            ["have"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N Y N N)},

         ;;passive
         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["have"]]
           ["VP"
            ["VBN"
             ["been"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["gotten"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["have"]]
           ["VP"
            ["VBN"
             ["gotten"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["has"]]
           ["VP"
            ["VBN"
             ["got"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["have"]]
           ["VP"
            ["VBN"
             ["got"]]
            ["VP"
             ["VBN"]]]]
          :depth 4
          :form (f N Y N Y)}
         
         ;;present simple
         ;;active
         {:pattern
          ["VP"
           ["VBZ"]]
          :depth 2
          :form (f N N N N)},

         {:pattern
          ["VP"
           ["VBP"]]
          :depth 2
          :form (f N N N N)}

         ;;passive
         {:pattern
          ["VP"
           ["VBZ"
            ["is"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N N N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["am"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N N N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["are"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N N N Y)}

         {:pattern
          ["VP"
           ["VBZ"
            ["gets"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N N N Y)}

         {:pattern
          ["VP"
           ["VBP"
            ["get"]]
           ["VP"
            ["VBN"]]]
          :depth 3
          :form (f N N N Y)}
         ]]
   
    ;;now sort from deepest to shallowest. this is so as to match
  ;;complex patterns first, since many of the complex ones
  ;;contain the simple one
    (reverse (sort-by :depth unsorted-patterns))))

(def *all-tense-aspect-names*
  (let [nonmodal-forms (map :form *verb-patterns*)
        modal-forms (map #(into % {:modal ["will"]})
                         (remove :past? nonmodal-forms))]
    (vec (distinct (map get-tense-aspect-name
                              (concat nonmodal-forms modal-forms))))))

(comment (defn tree->seq [t]
   (with-meta (cons (.value t)
                    (map tree->seq (.children t)))
     {:labeled-score-tree t}))

 (defn text->seq [text]
   (tree->seq (.apply parse/*parser* text))))


(defn split-conjunctions-at-level [head node type]
  ;;first determine if this node's children contains a splittable
  ;;conjunction
  (let [VP-children (children-of-type node type)]
    (if (and (has-descendants? node ["CC"])
             (> (count VP-children) 1))
      ;;process each VP child ONE BY ONE (not in parallel!)
      (loop [heads []
             rest-children VP-children]
        (if-let [child (first rest-children)]
          (let [other-children (doall (remove #(= child %) VP-children))
                other-children (doall (into other-children
                                            (children-of-type node "CC")))
                child-filter (proxy [edu.stanford.nlp.util.Filter] []
                               (accept [node]
                                 (if (some #(identical? node %) other-children)
                                   false
                                   true)))
                new-head (.prune head child-filter)]
            (recur (conj heads new-head) (rest rest-children)))
          heads))
      ;;else just return the head in a sequence
      (vector head))))

(defn splittable-at-level [node]
  "returns the type to split on or nil if not splittable"
  (let [splittable-type ["S" "VB" "VBZ" "VBG" "VBD" "VBN" "VBP" "VP"]
        children (map #(.value %) (.children node))]
    ;;simple test: CC plus more than one occurrence of one of the type
    (if (has-child-of-type? node "CC")
      (some (fn [type]
              (if (>= (count (filter #(= % type)
                                     children))
                      2)
                type))
            splittable-type))))

(defn split-or-recur [head node]
  "returns a seq of the splits or nil indicating no split made
If it can't split and it has children, it recurses for each child"
  (if-let [type (splittable-at-level node)]
    (split-conjunctions-at-level head node type)
    (some #(split-or-recur head %)
          (.children node))))

(defn split-conjunctions [parse]
  "Splits conjunctions at the verb and sentence levels"
  ;;loops so long as split-or-recur returns something
  (loop [heads [parse]]
    (if-let [replacement-heads
             (some (fn [head]
                     (if-let [new-heads (split-or-recur head head)]
                       {head new-heads}))
                   heads)]
      (do
        (recur (flatten (replace replacement-heads heads))))
      heads)))

(defn shorten-branches-at-node! [node]
  "if node has more than one child, this recurses on them. Otherwise it checks if the child in turn has an only child of the same value and if so, replaces the child with the grandchild. It then recurses on the child. if the node is a leaf, it returns"
  
  (let [children (.children node)]
    (case (count (.children node))
      0 nil
      1 (let [child (first children)
              grandchildren (.children child)]
          (if (and (== (count grandchildren) 1)
                   (= (.value (first grandchildren))
                      (.value child)))
            (.setChild node 0 (first grandchildren)))
          (shorten-branches-at-node! child))
      (map shorten-branches-at-node! (.children node)))))

(defn shorten-branches! [parse]
  "after split-conjunctions, there may be links in branches such as (VP (VP..)) that have no meaning. This shortens those destructively"
  (shorten-branches-at-node! parse)
  parse)


(defn extract-Ss-at-node! [head node]
  "First recurse on all children. Concat the sequences return by the children.
Check if the current node is S, if so, remove it from the parent, add it to the sequence and return that sequence"
  (let [children (.children node)
        sub-Ss (doall (mapcat (partial extract-Ss-at-node! head) children))]
    (if (= (.value node) "S")
      (let [parent (.parent node head)]
        (conj sub-Ss (.removeChild parent (.indexOf parent node))))
      sub-Ss)))

(defn extract-Ss [parse]
  "Takes a sentence and removes all subtrees headed by S. Then returns the modified sentence along with the removed S-headed structures. In other words, returns
a series of S-headed trees derived from 'parse' that do not contain S nodes other than the head"
  (let [head (.deepCopy parse)]
    (extract-Ss-at-node! head head)))

(defn label-eq [a b]
  (= (string/upper-case a) (string/upper-case b)))


(defn match-branch [parse-node pattern-node]
  "helper function for match-and-extract-verb. When this is called, the depth of these nodes  have already been matched at this level"
  ;;check that the labels match
  (if (label-eq (.value parse-node) (first pattern-node))
 ;;check that presence or absence of modals in the pattern is reflected in the parse
    (if (=
         (= "MD" (first (second pattern-node))) ;;MD always left child
         (boolean (some #(= "MD" (.value %))
                        (.children parse-node))))
      (cond
       ;;if we're at a leaf in both parse and pattern, we must have
       ;;reached an auxiliary verb. return it
       (and (.isLeaf parse-node)
            (empty? (rest pattern-node)))
       (vector (.value parse-node))
       ;;if we're at a preleaf in the parse and a leaf in the pattern
       ;;we must have reached the core verb, return it
       (and (.isPreTerminal parse-node)
            (empty? (rest pattern-node)))
       (vector (.value (first (.children parse-node))))
       ;;else if we're at a leaf in the parse there is a mismatch
       ;;or a leaf in the pattern
       (or (.isLeaf parse-node)
           (empty? (rest pattern-node)))
       nil
       ;;otherwise loop through the parse children, matching the labels
       ;;to those in the pattern children, ignoring MD
       ;;if we reach the end of the parse children without having
       ;;processed the pattern children (of which there is 1 or 2),
       ;;there is a mismatch
       :else
       (loop [parse-children (.children parse-node)
              pattern-children (rest pattern-node)
              results nil]
         (cond
          ;;skip modals
          (= "MD" (first (first pattern-children)))
          (recur parse-children (rest pattern-children) results)
          ;;matched all patterns
          (empty? pattern-children)
          results
          ;;finished parse-children before pattern-children, mismatch
          (empty? parse-children)
          nil
          ;;otherwise call match-branch with the left-most children of
          ;;both, and if there is a match, add the results, and
          ;;move along both lists of children
          ;;if no match, just move along the parse-children
          :else
          (if-let [result (match-branch (first parse-children)
                                        (first pattern-children))]
            (recur (rest parse-children)
                   (rest pattern-children)
                   (concat results result))
            (recur (rest parse-children)
                   pattern-children
                   results))))))))


(defn extract-modals [parse]
  "return a list of strings which are the text of the modals. the label of 'parse' should be 'VP'"
  (map #(.value (first (.children %)))
       (filter #(label-eq "MD" (.value %))
               (.children parse))))

(defn modify-get-passives! [parse]
  "the stanford parses generates a (VP (S (VP (VBN)))) rightmost tail in
the trees for get passives. It doesn't do so when there is a conjunction in that member (e.g. They are getting bought and sold). This functions removes that S and reattaches the branch"
  ;;check if we are at a terminal branch
  (if (not (.isLeaf parse))
    ;;recurse first, then check this branch
    (do
      (doseq [child (.children parse)]
        (modify-get-passives! child))
      (if (and
           (= (.value parse) "VP")
           (let [child (last (.children parse))
                 child-children-count (.numChildren child)]
             (and
              (= (.value child) "S")
              (== child-children-count 1)
              (let [child (first (.children child))
                    child-children-count (.numChildren child)]
                (and
                 (= (.value child) "VP")
                 (== child-children-count 1)
                 (= (.value (first (.children child))) "VBN"))))))
        (do
          (println "modifying get-passive")
          (println parse)
          (.setChild parse
                     (dec (.numChildren parse))
                     (first (.children (last (.children parse))))))
        parse))))

(defn match-and-extract-verb [parse pattern]
  "if the parse matches the pattern, returns a dictionary with grammatical information. Last item in the sequence will be the verb. If the first item is a sequence, it is a list of modal, the rest of the items will be aux verbs"
  ;;first do depth check
  (let [dparse (.depth parse)
        dpattern (:depth pattern)]
    (cond
     (< dparse dpattern) nil ;can't possibly match
     
     (>= dparse dpattern) ;may match here or somewhere deeper in parse
     (if-let [verb (match-branch parse (:pattern pattern))]
       (into (:form pattern)
             {:verb verb
              :modal (extract-modals parse)})
       (some #(match-and-extract-verb % pattern)
           (.children parse))))))

(comment (== dparse dpattern)   ;may match, check at this depth
      (if-let [verb (match-branch parse (:pattern pattern))]
        (into (:form pattern)
              {:verb verb
               :modal (extract-modals parse)})))

(defn extract-verbs [parse]
  (let [ps (modify-get-passives! (.deepCopy parse))
        ps (extract-Ss parse)
        ps (mapcat split-conjunctions ps)]
    (doseq [p ps]
      (shorten-branches! p))
    (remove nil?
     (map (fn [p]
            (some #(match-and-extract-verb p %)
                  *verb-patterns*))
          ps))))

(defn verb-test [text]
  (extract-verbs (.apply parse/*parser* text)))


;;;MODALS
(def *all-modals*
  ["can" "could" "dare" "may" "might" "must" 
   "ought" "shall" "should" "will" "would"])

(def *contracted-modals*
  ["ca" "mus" "sha" "wo"])

(def *contracted-modal-map*
  {"ca" "can", "mus" "must", "sha" "shall", "wo" "will"})

(defn extract-modals-at-node [node]
  "if a leaf, return an empty seq, if label of node is MD, return that value of the child, otherwise recur on children and gather the results in a list."
  (cond
   (.isLeaf node) []
   (= "MD" (.value node)) [(.toLowerCase (.value (first (.children node))))]
   :else (mapcat extract-modals-at-node (.children node))))

(defn extract-modals [parse]
  "returns a list of modals in a parse, resolving contractions into the base form"
  (map (fn [m]
         (if (contains? *contracted-modal-map* m)
           (*contracted-modal-map* m)
           m))
       (extract-modals-at-node parse)))

;;;HIGH-FREQUENCY VERBS
(def *high-freq-verbs*
  ["have" "go" "take" "do" "say" "look" "know" "see" "give"
   "think" "come" "find" "get" "make" "use"])

(def *high-freq-verb-inflections-map*
  (let [inflections
        [["has" "had"] "have"
         ["goes" "went" "gone"] "go"
         ["takes" "took" "taken"] "take"
         ["does" "did" "done"] "do"
         ["says" "said"] "say"
         ["looks" "looked"] "look"
         ["knows" "knew" "known"] "know"
         ["sees" "saw" "seen"] "see"
         ["gives" "gave" "given"] "give"
         ["thinks" "thought"] "think"
         ["comes" "came"] "come"
         ["finds" "found"] "found"
         ["gets" "got" "gotten"] "get"
         ["makes" "made"] "make"
         ["uses" "used"] "use"]]
    ;;now convert those into maps, with multiple keys (the inflections)
    ;;referring to the same value (the base form)
    (apply merge
           (apply concat
                  (for [[infls base] (partition 2 inflections)]
                    (for [infl infls]
                      {infl base}))))))

(defn high-freq-verb-base-form [form]
  "either returns the base form, or nil if it's not in the list"
  (if (contains? *high-freq-verb-inflections-map* form)
    (*high-freq-verb-inflections-map* form)
    (some #(if (= % form) form) *high-freq-verbs*)))

(defn extract-high-freq-verbs [parse]
  "return a list of all the BASE FORMS of the high frequency verbs found in parse"
  (let [verbs (extract-verbs parse)]
    (remove nil?
            (for [verb verbs]
              (high-freq-verb-base-form (last (:verb verb)))))))

(defn extract-high-freq-verbs-2 [verbs]
  "return a list of all the BASE FORMS of the high frequency verbs found in the sequence of verb mappings 'verb'"
  (remove nil?
          (for [verb verbs]
            (high-freq-verb-base-form (last (:verb verb))))))