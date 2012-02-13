;;note 'arg' refers to the system that distinguishes anaphora from lexical NP, arg-struct refers
;;to just looking at different verb types in terms of the argument patterns they accept

(ns thesis.train-arguments
  (:require
   [thesis.data :as data]
   [thesis.arguments :as arg]
   [thesis.tools :as tools]
   [clj-ml.data :as mld]
   [clj-ml.classifiers :as mlc]
   [incanter.stats :as stats]
   [incanter.core :as incanter]
   [clojure.string :as string])
  (:import
   [weka.classifiers.trees RandomForest J48]
   [weka.classifiers.bayes NaiveBayesMultinomial]
   [weka.classifiers.functions MultilayerPerceptron]
   java.io.File))

(defn load-all-corpora-deps [L1]
  "return a seq of seqs of seqs of deps of a given L1. (instances->sentences)"
  (let [corpora ({:es data/*es-corpora* :en data/*en-corpora*} L1)]
    (mapcat (fn [corpus-info]
              (let [corpus (:corpus corpus-info)]
                (map (fn [filename]
                       (println corpus " " filename)
                       (data/load-deps corpus filename))
                     (:filenames corpus-info))))
            corpora)))

(defn case-ins-eq? [a b]
  (= (string/upper-case a) (string/upper-case b)))

(defn relative-arg-freqs-1 [args]
  "used by relative-arg-freqs, takes a list of argument attributes 'ao' 'aiO' etc, counts each type and divides that number by the frequency of tha particular structure"
  (let [structure-counts
        (reduce into
                (for [s arg/*all-argument-structures*]
                  {s (apply + (for [a args]
                                (if (case-ins-eq? a s) 1 0)))}))
        arg-freqs (frequencies args)
        arg-rel-freqs (zipmap (keys arg-freqs)
                              (for [[attr freq] arg-freqs]
                                (double (/ freq (structure-counts (string/lower-case attr))))))]
    arg-rel-freqs))

(defn relative-arg-freqs [deps]
  "takes a sequence of sequences of deps. returns a map where the keys are the arg structures and the values are the relative frequencies of that particular arg structure"
  (let [args (mapcat arg/extract-arguments deps)
        arg-rel-freqs (relative-arg-freqs-1 args)]
    
    ;;now add zeros for any args
    (apply merge (map (fn [a]
                        (if (contains? arg-rel-freqs a)
                          {a (arg-rel-freqs a)}
                          {a  0.0}
                          ;;{a [(arg-rel-freqs a)]};;replace above with this for t-test
                          ;;{a  [0.0]}
                          ))
                      arg/*all-argument-attributes*))))

(defn relative-arg-struct-freqs [deps]
  "ignores whether the arguments are anaphora and just looks at relative frequencies of the different
argument structures. there are six: s,ao,aio,aoc,p,pc"
  (let [args (map string/lower-case (mapcat arg/extract-arguments deps))
        arg-count (count args)
        arg-freqs (frequencies args)
        arg-rel-freqs (apply merge (for [[struct freq] arg-freqs]
                                     {struct (double (/ freq arg-count))}))]
    ;;now add zeros for any args that aren't used
    (apply merge (map (fn [a]
                        (if (contains? arg-rel-freqs a)
                          {a (arg-rel-freqs a)}
                          {a  0.0}))
                      arg/*all-argument-structures*))))

(defn make-arg-dataset []
  "Makes a dataset with numerical attributes of argument relative frequencies"
  (let [ds (mld/make-dataset "Arguments"
                             (conj arg/*all-argument-attributes*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn make-arg-struct-dataset []
  "Makes a dataset with numerical attributes of argument relative frequencies"
  (let [ds (mld/make-dataset "Arguments Structures"
                             (conj arg/*all-argument-structures*
                                   {"L1" ["es" "en"]})
			  100)]
    (mld/dataset-set-class ds "L1")
    ds))

(defn add-to-arg-dataset [ds arg-freqs L1]
  "adds an instance using the data found in the map arg-freqs"
  (println (count arg-freqs))
  (mld/dataset-add ds (concat (map arg-freqs arg/*all-argument-attributes*)
                              [({:en "en" :es "es"} L1)])))

(defn add-to-arg-struct-dataset [ds arg-freqs L1]
  "adds an instance using the data found in the map arg-freqs"
  (mld/dataset-add ds (concat (map arg-freqs arg/*all-argument-structures*)
                              [({:en "en" :es "es"} L1)])))

(defn make-arg-dataset-from-all-corpora []
  "makes a dataset using arg relative frequencies all of the corpora"
  (let [ds (make-arg-dataset)
        es-freqs (map relative-arg-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-freqs (map relative-arg-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :en)))]
    (doseq [a es-freqs]
      (add-to-arg-dataset ds a :es))
    (doseq [a en-freqs]
      (add-to-arg-dataset ds a :en))
    ds))

(defn make-arg-struct-dataset-from-all-corpora []
  "makes a dataset using arg relative frequencies all of the corpora"
  (let [ds (make-arg-struct-dataset)
        es-freqs (map relative-arg-struct-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-freqs (map relative-arg-struct-freqs
                      (map (partial remove empty?) (load-all-corpora-deps :en)))]
    (doseq [a es-freqs]
      (add-to-arg-struct-dataset ds a :es))
    (doseq [a en-freqs]
      (add-to-arg-struct-dataset ds a :en))
    ds))

(defn train-and-test [dataset]
  (let [cl (NaiveBayesMultinomial.)]
    ;(.setNumTrees cl 100)
    (println (mlc/classifier-train cl dataset))
    (println (mlc/classifier-evaluate cl :cross-validation dataset 20))
    cl))

(defn run-arguments-t-test []
  (let [es-freqs (doall
                  (apply (partial merge-with concat)
                         (map relative-arg-freqs
                              (map (partial remove empty?) (load-all-corpora-deps :es)))))
        en-freqs (doall
                  (apply (partial merge-with concat)
                         (map relative-arg-freqs
                              (map (partial remove empty?) (load-all-corpora-deps :en)))))]
    (for [arg-pattern arg/*all-argument-attributes*]
      ;;incanter doesn't like it if all zeros are passed to it
      (if (some #(not= 0 %) (concat (es-freqs arg-pattern) (en-freqs arg-pattern)))
        (let [test-results (stats/t-test (es-freqs arg-pattern)
                                         :y (en-freqs arg-pattern))]
          (print "\n*****\n"
                 arg-pattern "\n"
                 test-results "\n")
          (into test-results {:arg-pattern arg-pattern}))))))

(defn filter-t-test-results [results]
  (->> results
       (remove (fn [{[lower upper] :conf-int}] (or (nil? lower) (nil? upper))))
       (remove (fn [{[lower upper] :conf-int}] (and (< lower 0) (> upper 0))))))

(defn print-t-test-results [results]
  (doseq [r (filter-t-test-results results)]
    (println (:arg-pattern r) " --> " (if (> (:y-mean r) (:x-mean r))
                                        "EN"
                                        "ES"))))

(defn get-ind-args [deps]
  "For a single instance of deps (multiple sentences), return all individual arguments"
  (let [args (mapcat arg/extract-arguments deps)]
    (flatten (for [a args] (map str (seq a))))))

(defn get-per-arg-counts []
  (let [es-args (mapcat get-ind-args
                     (map (partial remove empty?) (load-all-corpora-deps :es)))
        en-args (mapcat get-ind-args
                        (map (partial remove empty?) (load-all-corpora-deps :en)))
        es-freqs (frequencies es-args)
        en-freqs (frequencies en-args)]
    {:en en-freqs :es es-freqs}))

(defn get-arg-struct-counts []
  (let [es-args (mapcat arg/extract-arguments
                     (mapcat (partial remove empty?) (load-all-corpora-deps :es)))
        en-args (mapcat arg/extract-arguments
                        (mapcat (partial remove empty?) (load-all-corpora-deps :en)))
        es-freqs (frequencies es-args)
        en-freqs (frequencies en-args)]
    {:en en-freqs :es es-freqs}))

(defn- dump-dataset [ds path]
  (with-open [outp (-> (File. path)
                       java.io.FileOutputStream.
                       java.io.ObjectOutputStream.)]
    (.writeObject outp ds)))

(defn- load-dataset [path]
  (with-open [inp (-> (File. path)
                        java.io.FileInputStream.
                        java.io.ObjectInputStream.)]
    (.readObject inp)))

(defn- dump-arg-dataset [ds]
  (dump-dataset ds "../data/train-args.dataset"))

(defn- dump-arg-struct-dataset [ds]
  (dump-dataset ds "../data/train-arg-structs.dataset"))

(defn- load-arg-dataset []
  (load-dataset "../data/train-args.dataset"))

(defn- load-arg-struct-dataset []
  (load-dataset "../data/train-arg-structs.dataset"))

(defn- calc-valency-attrs [inst-as]
  [;; "one"
   (apply + (map inst-as [:p :m :e :s]))
   ;; "two"
   (apply + (map inst-as
                 [:ao :pc :sc :bk :ak :bo :mc :pk :mk :ec]))
   ;; "three"
   (apply + (map inst-as
                 [:aio :bio :aoc :boc]))])

(defn- permute-n-lex [strct n]
  "takes a string such as aio and returns only those permutations with n lexical arguments"
  (map keyword
       (let [perms (arg/permute-arg-structure strct)]
         (filter (fn [p]
                   (if (== n (apply + (for [c p]
                                        (if (= (str c) (string/upper-case c)) 1 0))))
                     true false))
                 perms))))

(defn- calc-num-attrs [inst-a inst-as]
  "Attributes indicating how much clauses contains zero,one,two, or three lexical attributes"
  (vec (letfn [(f [strct n]
                 (let [p (permute-n-lex strct n)]
                   (* ((keyword strct) inst-as)
                      (apply + (map inst-a (map keyword p))))))]
         (for [n [0 1 2 3]]
           (apply + (for [strct arg/*all-argument-structures*]
                      (f strct n)))))))

(defn- permute-arg-lex [strct a]
  "takes a string such as aio and returns only those permutations with the arg a is lexical"
  (map keyword
        (let [lex-a (string/upper-case a)
              perms (arg/permute-arg-structure strct)]
          (filter (fn [p]
                    (some (fn [c]
                            (= (str c) lex-a)) p))
                  perms))))

(defn calc-lex-attrs [inst-a inst-as]
  (letfn [(f [strct a]
            (let [p (permute-arg-lex strct a)]
              (* ((keyword strct) inst-as)
                 (apply + (map inst-a (map keyword p))))))]
    (let [vals
          [ ;;intr-subj, s or e, not sc or ec
           (+ (f "s" "s")
              (f "e" "e"))
           ;;trans-subj, a or b
           (+ (apply + (map #(f % "a") ["ao" "aoc" "aio" "ak"]))
              (apply + (map #(f % "b") ["bo" "boc" "bio" "bk"])))
           ;;i-obj, i
           (apply + (map #(f % "i") ["aio" "bio"]))
           ;;d-obj, o, k
           (+ (apply + (map #(f % "o") ["aoc" "aio" "ao" "bo"]))
              (apply + (map #(f % "k") ["ak" "bk"])))
           ;;pass-subj, p, m
           (+ (apply + (map #(f % "p") ["p" "pc" "pk"]))
              (apply + (map #(f % "m") ["m" "mc" "mk"])))
           ;;compl, c
           (apply + (map #(f % "c") ["pc" "sc" "aoc" "boc"]))
           ;;cop-subj, s, e, only sc and ec
           (+ (f "sc" "s")
              (f "ec" "e"))
           ]]
      ;;normalize them here
      (let [sum (apply + vals)]
        (map #(/ % sum) vals))
      ))
  )

(defn- arg->valency [as-ds]
  "Converts an arg-struct dataset into one with just four attributes, corresponding to the number of lexical arguments in the verbal clause. The associated values are the percentage of finite verbal clauses with that number."
  (let [num-ds (mld/make-dataset "Argument Valency"
                             [:mono :di :tri
                              {:L1 [:es :en]}] (.numInstances as-ds))]
    (mld/dataset-set-class num-ds :L1)
    (doseq [inst-as (mld/dataset-as-maps as-ds)]
      (let [new-inst (mld/make-instance num-ds
                      (concat (calc-valency-attrs inst-as) [(:L1 inst-as)]))]
        (.add num-ds new-inst)))
    num-ds))

(defn- arg->num-lex [as-ds a-ds]
  "Converts an arg dataset and an arg-struct dataset into one with just four attributes, corresponding to the number of lexical arguments in the verbal clause. The associated values are the percentage of finite verbal clauses with that number."
  (let [num-ds (mld/make-dataset "Lexical Argument Count"
                             [:zero :one :two :three
                              {:L1 [:es :en]}] (.numInstances as-ds))]
    (mld/dataset-set-class num-ds :L1)
    (doseq [[inst-as inst-a]
            (partition-all 2 (interleave (mld/dataset-as-maps as-ds) (mld/dataset-as-maps a-ds)))]
      (let [new-inst (mld/make-instance
                      num-ds
                      (concat (calc-num-attrs inst-a inst-as) [(:L1 inst-a)]))]
        (.add num-ds new-inst)))
    num-ds))

(defn- arg->lex-role [as-ds a-ds]
  "Converts an arg dataset and an arg-struct dataset into one with an attribute for each argument role: :intr-subj, tran-subj, i-obj, d-obj, pass-subj, compl, cop-subj."
  (let [num-ds (mld/make-dataset "Lexical Argument Roles"
                                 [:intr-subj :trans-subj :i-obj
                                  :d-obj :pass-subj :compl :cop-sub
                              {:L1 [:es :en]}] (.numInstances as-ds))]
    (mld/dataset-set-class num-ds :L1)
    (doseq [[inst-as inst-a]
            (partition-all 2 (interleave (mld/dataset-as-maps as-ds) (mld/dataset-as-maps a-ds)))]
      (let [new-inst (mld/make-instance
                      num-ds
                      (concat (calc-lex-attrs inst-a inst-as) [(:L1 inst-a)]))]
        (.add num-ds new-inst)))
    num-ds))

(defn- arg->lex-num-val [as-ds a-ds]
  (let [num-ds (mld/make-dataset "Lexical Argument Count and Roles and Verb Valency"
                                 [:mono :di :tri
                                  :zero :one :two :three
                                  :intr-subj :trans-subj :i-obj
                                  :d-obj :pass-subj :compl :cop-sub
                              {:L1 [:es :en]}] (.numInstances as-ds))]
    (mld/dataset-set-class num-ds :L1)
    (doseq [[inst-as inst-a]
            (partition-all 2 (interleave (mld/dataset-as-maps as-ds) (mld/dataset-as-maps a-ds)))]
      (let [new-inst (mld/make-instance
                      num-ds
                      (concat (calc-valency-attrs inst-as) (calc-num-attrs inst-a inst-as)  (calc-lex-attrs inst-a inst-as) [(:L1 inst-a)]))]
        (.add num-ds new-inst)))
    num-ds))

(defn build-trees [as-ds a-ds]
  "Takes the arg struct and arg dataset sets and return a vector of three J48 classifiers, based on valency, num of lexical argument, and lex argument role."
  (let [valen (arg->valency as-ds)
        num-lex (arg->num-lex as-ds a-ds)
        lex-role (arg->lex-role as-ds a-ds)
        clv (J48.)
        cln (J48.)
        cll (J48.)]
    (mlc/classifier-train clv valen)
    (spit "../results/c45-val-accuracy.txt" (mlc/classifier-evaluate clv :cross-validation valen 20))
    (mlc/classifier-train cln num-lex)
    (spit "../results/c45-num-lex-accuracy.txt" (mlc/classifier-evaluate cln :cross-validation num-lex 20))
    (mlc/classifier-train cll lex-role)
    (spit "../results/c45-lex-role-accuracy.txt" (mlc/classifier-evaluate cll :cross-validation lex-role 20))
    (tools/write-dtree clv "../opus/c45-val.dot")
    (tools/write-dtree cln "../opus/c45-num-lex.dot")
    (tools/write-dtree cll "../opus/c45-lex-role.dot")
    [clv cln cll]))
