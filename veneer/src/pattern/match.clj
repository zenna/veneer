(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match
  (:require [clojure.zip :as zip]))

; TODO:

; 1. We need a set of reductions, or rules
; each rule is a relation.
; e.g. (-> (square x) (* x x))

; A binary relation is a set of pairs.  For instance sqrt is a relation on the reals, [(9,3)(16,4)] etc.

; (-> (square x) (* x x))

; let's say the alphabet is a b c d e f
; relation is (a b) (c d) (e f) (b d)

; A binary relation is fully specified by the combination of an expression and
; a transformer.

; The binary relation implies a directed graph.

; The transformer then traverses this graph. to evaluate the program.
; How can the transformer both define and traverse the graph.
; It defines in it in the same sense that a moveset procedure of a local search algorithm defines a configuration space - the graph is defined implicitly by all the moves it could possibly do.  But the dynamics of the algorithm traverses only a subset of these states.
; What does possibly mean?
; Suppose I have a rewrite rule that will never be excuted, but is a rule nontheless, for example because there is another rule of higher precedence which would always match when it matches, does this rule define edges on the graph?
; This is a philosophical question, nearing meaningless since there is no 'real' graph.

(def a-rule '(-> (square x) (* x x) (and (pos? x))))
(def primitive-apply-rule
  '(-> (f x1 .. xn) ~(f x1 .. x2) (and (function? f)
                                       (primitive? f)
                                       (evaluated? xi))))

(def rand-rule
  '(-> (rand x y) (->interval-abo x y)))

(def convex-hull-rule
  '(⊑ (or x y) ~(convex-hull x y) (abo? x)))

(def rules [a-rule primitive-apply-rule rand-rule convex-hull-rule])
(def term '(+ 3 4 (rand 0 1) (rand 3 4)))

; (+ 3 4 (rand 0 1) (rand 3 4))

(defprotocol Iterator
  "This iterator is a purely functional version of an iterator ala C++
   The purpose is to abstract away the mechanics of iteration for better
   modularity.
   Suppose you have a pattern matching algorithm and you want to parameterise
   whether you are matching against every single node or just subgraphs.
   Iteration is done with (step iterator) which returns a new iterator,
   with some kind of pointer (this is a protocol so implementation details) to
   the next location.
   Since the iterator contains lots more information than you may want for
   any particular algorithm that uses it, we must use the realise function to
   get the actual value"
  (step [iterator])
  (end? [iterator])
  (realise [iterator]))

(defrecord NodeIterator
  "Iterates through all nodes of an expression depth-first.
   e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
        (+ 3 4 (rand 0 1) (rand 3 4)), +, 3, (rand 0 1), rand, 0, .."
  [tree zipped-tree])

(defrecord SubtreeIterator
  "Iterates through all subtrees of an expression depth-first.
   e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
        (+ 3 4 (rand 0 1) (rand 3 4)), (rand 0 1), (rand 3 4)"
  [tree zipped-tree])

(defn node-iterator
  "Node iterator factory"
  [exp]
  (NodeIterator. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(defn subtree-iterator
  "Subtree iterator factory"
  [exp]
  (SubtreeIterator. exp (zip/zipper coll? seq (fn [_ c] c) exp)))

(def abstract-zip-iterator-impl
  "Abstract implementation for iterators built upon clojure/zip"
  {:step
    (fn [iterator]
      (assoc iterator :zipped-tree (zip/next (.zipped-tree iterator))))

   :end?
   (fn [iterator]
     (zip/end? (.zipped-tree iterator)))

    :realise
    (fn [iterator]
      (first (.zipped-tree iterator)))})

; Node iterator is the same as the abstract iterator
(extend NodeIterator
  Iterator
  abstract-zip-iterator-impl)

(defn repeat-until
  "Keep doing (f x), where x is initially init-value until 
  (pred (f x)) is satisfied, then return (f x).
   e.g. (repeat-until 3 inc (partial > 7)"
  [init-value f pred?]
  (loop [value (f init-value)]
    (if (pred? value)
        value
        (recur (f value)))))

(defn loop-until
  "Loop through coll until (f elem) is not nil.
   then return (f elem).
   If no matches for any then return nil"
  [f coll]
  (some #(if (f %) %) coll))

(extend SubtreeIterator
  Iterator
  (assoc abstract-zip-iterator-impl
    ; Keep doing zip/next until I get to a branch
    :step
    (fn [iterator]
      (let [zip-tree
            (repeat-until (.zipped-tree iterator)
                          zip/next
                          #(or (zip/end? %) (zip/branch? %)))]
        (assoc iterator :zipped-tree zip-tree)))))

; TODO
(defn pat-match-many
  "Go through a list of rules
   if any of them match, do the rewrite, else nil"
  [exp rules]
  (loop-until (partial pat-match exp) rules))
  
(defn eager-transformer
  "This is an eager transformer.
   It matches the rules in order and applies first match"
  [rules exp]
  (loop [iterator (subtree-iterator exp)]
    (end? iterator) ; no rules matched
    nil

    (if-let [match (pat-match-many (realise iterator) rules)]
      match
      (recur (step iterator)))))

(defn rewrite
  "Rewrite an expression using a relation"
  [exp relation]
  (repeat-until exp relation nil?))

(comment
  (def a-rule '(-> (square x) (* x x) (and (pos? x))))
  (def primitive-apply-rule
    '(-> (f x1 .. xn) ~(f x1 .. x2) (and (function? f)
                                         (primitive? f)
                                         (evaluated? xi))))

  (def rand-rule
    '(-> (rand x y) (->interval-abo x y)))

  (def convex-hull-rule
    '(⊑ (or x y) ~(convex-hull x y) (abo? x)))

  (def rules [a-rule primitive-apply-rule rand-rule convex-hull-rule])
  (def term '(+ 3 4 (rand 0 1) (rand 3 4)))

  (def transformer (partial eager-transformer rules))
  (def evaluated-term (rewrite term eager-transformer))

; ; 1. Decide what is a pattern, rewrite and rule
; ; 2. Decide whether we are working with an equational system or a rewrite system
; ; 3. Figure out how to do variable argument lists

; ; Discussion Summary
; ; There are two parts to this system.
; ; The first is a set of relations, for instance (+ x y) = (+ y x), (square x) -> (* x x), x ⊑ (convex-hull x).
; ; The latter is a transformer (equally an interpeter, evaluator or rewriter)
; ; The combination of the relations together with the transformer define a graph.
; ; Each node of this graph is a term and nodes are connected by the transformers interpretation of the relations.

; ; The evaluation of purely functional lisp programs can be thought of as a special case.
; ; There each term is a tree, rules could be of the form
; ; (f x y) -> ~(f x y).
; ; Typical transformers interpret the arrow as a rewrite, and typically operate
; ; in a strictly ordered fashion in order to reduce an expression to its normal form.

; ; In approximate or abstract interpretation, as well as other domains there are other criteria.
; ; We may represent abstraction rules as, for example, abstraction relations, e.g. (or x y z) ⊑ (convex-hull x y z).
; ; A sound transformer shall then have to recognise that it can replace any term with its approximation.
; ; An unsound transformer might even do the opposite.
; ; There may be no normal form, instead there may be a variety of normal forms.
; ; There are two ideas that need to be modelled - there are goal and non goal states in that sense that we hope to fully evaluate the program, and 2) among all states (including the goals), there is variable associated cost.
; ; This may be a measure of how much memory is in use, how many abstract objects,
; ; how accurate the approximation is etc.

; ; It may be desirable for some interpreters to be able to back track.
; ; Among the multiple ways of doing this, one that seems most elegant and compatible with an MDP prespective on evaluation is to modify all existing rewrite rules.
; ; These rewrite would say, whenever I transform an object, I shall store its history with it, i.e. what it was before.
; ; I then have a general rule which matches any object with a history and can revert it.

; ; Discussion
; ; - What's a rule
; ; Equational reasoning is concerned with a restricted class of first-order languages: the only predicate symbol is equality.
; ; Term rewriting restricts this furher to unidirectionality.

; ; The unidirectionality gives us a direction to ensure our computations terminate, and that under some other considerations, the order of evaluation does not matter.

; ; In abstract interpretation the choices made do matter, and do affect the result.
; ; We're encoding these choices as rewrites.

; ; Choice 1.
; ; A pattern is a predicate which matches (i.e. returns true) on a given expression or not.
; ; A rewrite is a replacement for that expression to be applied if the pattern matches.

; ; (+ x y) -> (+ y x)

; ; (+ a (+ b c)) => 

; ; You could say a rewrite rule is a pair (predicate, replacement)
; ; where the replacement is just a value.
; ; Typically though, we want to extract some values from our pattern and reuse them in the rewrite, hence we make the replacement some function of the bound variables

; ; pattern::exp -> bool
; ; replacement::bindings -> exp

; ; But then what about equalionality

; ; (+ x y) = (+ y x)

; ; The lhs and rhs act both as patterns, i.e. patterns to be matched, and rewrites, there is no difference.

; ; so a pattern in a sense has a dual functional interpretation.
; ; 1. it is a predicate which will match or not match a variable
; ; 2. its a value which can replace another

; ; The recursive application of of rules should be the responsibility of the rewrite or deduction system, not that of the pattern itself.

; ; - Abstract interpretation choices
; ; We can represent choices to be made in abstract interpretation as rewrite rules
; ; e.g. x ⊑ (convex-hull x) when ()

; ; This has implications for the rewriting.
; ; If the rewriter is sound it should only perform rewrites which approximate.


; ; - History: Rewrites vs Equations
; ; When evaluating a program, in particular when we have to make several choices
; ; regarding which abstractions to use and to what precision, there may be
; ; times where we want to undo.
; ; We may wish to revert our decisions and make better ones in light of new evidence.
; ; Rewriting a program is inherently destructive, information is lost along the
; ; way.  In order to be able to backtrack there seem to be two viable alternatives.
; ; 1. The agent stores his history, and other information as he sees fit.
; ; 2. We do not do rewriting in the stricted sense of the term, instead
; ; the program is one point in a discrete state space, adjoined by transitions
; ; made possible through rewrites.

; ; Practical difference
; ; Rewrites are not directional, they are equations.
; ; It is no longer pattern/rewrite

; ; e.g.
; ; square x = x * x is bidirectional.
; ; sqrt x = ~(sqrt [x]) when (positive? x)

; ; mul x y = ~(* x y)

; ; If I have 4, I could convert it to (mul 2 2), but multiplication is a one
; ; way function.

; ; Pros of 1.
; ; We keep a standard rewrite system, which can be used for other purposes.

; ; - Performance


; ; - Precedence and Decision Making
; ; Precedence is important in rewrite systems due to conflicts.
; ; There are at least two types of conflict.
; ; Both of these occur when an expression is mathed by more than one pattern (pattern + rewrite).
; ; This could be the same pattern matching multiple times in a single expression:

; ; square x -> x * x;
; ; => (square (square x))

; ; or different patterns matching the expression
; ; double x -> x + x
; ; (double (square x))

; ; The previous two examples were not desctuctive, in the sense that
; ; performing either of the rewrites did not invalidate the other pattern, and
; ; hence the order of rewrites has no effect on the fina result.
; ; There are destructive conflicts however:
; ; (f (/ x 0) _) -> undefined
; ; => (square (/ 5 0))

; ; Rewrite systems typically do not have much machinery in the way of ordering.

; ; - A DSL which makes it convenient to write these rewrite rules
; ; We can use macros to define a pure like language for rewriting.

; ;; Parse DSL
; ; We create pure-lang like DSL for more concise rule writing
; (defn split-coll-at
;   "Split vector along 'at'"
;   [coll at]
;   (loop [split-colls [] coll coll]
;     (println "coll is" coll)
;     (let [index (.indexOf coll at)]
;       (if (= index -1)
;           (conj split-colls coll)
;           (recur (conj split-colls (subvec coll 0 index)) 
;                  (subvec coll (inc index)))))))


; ; (defmacro defrule
; ;   [docstring & args]
; ;   {:pre [in? args '->]}

; ; ;; Rule Type
; ; (defrecord Rule
; ;   "A

; ;    An iterator - (iterate exp) returns the next element.
; ;    How to do this in a functional way, e.g. if I want to do
; ;    bfs.
; ;    Returns nil when nothing left to iterate"
; ;   [pattern rewrite condition iterate]

; ; (defn rule
; ;   "Create a rule, e.g. '((square x) -> x * x)"
; ;   [rule]

(def fail
  "indicates pat-match failure"
  nil)

(def no-bindings
  "indicates pat-match success wo variables"
  {})

(defn get-binding
  "Find a variable value pair in the binding list"
  [var bindings]
  (bindings var))

; (defn binding-val (binding))

(defn extend-bindings
  "Adds a var binding to binding map"
  [var input bindings]
  (assoc bindings var input))

(defn match-variable
  "Does variable match input?"
  [var input bindings]
  (let [binding-val (get-binding var bindings)]
    (cond 
      (nil? binding-val) (extend-bindings var input bindings)
      
      (= input binding-val) bindings
      
      :else fail)))

(defn variable?
  "is x a variable (a symbol starting with ?"
  [x]
  (and (symbol? x) (.startsWith (name x) "?")))

; (defn segment-pattern?
;   "is this a segment matching pattern?"
;   [pattern]
;   (and (list? pattern)
;        (.startsWith (name (first pattern)) "?*")))

; TODO
(defn pat-rewrite
  "Determine whether a pattern matches, if so, rewrite accordingly.
   Else return nil"
  [exp rule]
  nil)

(defn pat-match
  "Match pattern against input in the context of the binding"
  ([pattern input] (pat-match pattern input no-bindings))
  ([pattern input bindings]
  (println "pattern matching" pattern "-" input)
  (cond
    (= bindings fail) fail
    
    (variable? pattern)
    (match-variable pattern input bindings)

    (= pattern input)
    bindings

    ; (segment-pattern? pattern)
    ; (segment-match pattern input bindings)

    (and (list? pattern) (list? input))
    (pat-match (rest pattern) (rest input)
               (pat-match (first pattern) (first input)
                           bindings))
    :else
    fail)))

; (comment
;   (->Rule '(square x) '(* x x) )
;   (defrule square "Square" x -> x * x)
;   (pat-match '(square ?x) '(+ (square 10) (square 20)))

;   (defrule primitive-f
;     "evaluate primitive functions"
;     f x1 .. xn -> (f x1 .. x2) where (and (function? f)
;                                           (primitive? f)
;                                           (evaluated? xi))))