(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match
  (:require [clojure.zip :as zip]))

; There are two parts to this system.
; The first is a set of sentences, for instance (+ x y) = (+ y x), (square x) -> (* x x), x ⊑ (convex-hull x).
; The latter is a transformer (equally an interpeter, evaluator or rewriter)
; The combination of the relations together with the transformer define a graph.
; Each node of this graph is a term and nodes are connected by the transformers interpretation of the relations.

; The evaluation of purely functional lisp programs can be thought of as a special case.
; There each term is a tree, rules could be of the form
; (f x y) -> ~(f x y).
; Typical transformers interpret the arrow as a rewrite, and typically operate
; in a strictly ordered fashion in order to reduce an expression to its normal form.

; In approximate or abstract interpretation, as well as other domains there are other criteria.
; We may represent abstraction rules as, for example, abstraction relations, e.g. (or x y z) ⊑ (convex-hull x y z).
; A sound transformer shall then have to recognise that it can replace any term with its approximation.
; An unsound transformer might even do the opposite.
; There may be no normal form, instead there may be a variety of normal forms.
; There are two ideas that need to be modelled - there are goal and non goal states in that sense that we hope to fully evaluate the program, and 2) among all states (including the goals), there is variable associated cost.
; This may be a measure of how much memory is in use, how many abstract objects,
; how accurate the approximation is etc.

; It may be desirable for some interpreters to be able to back track.
; Among the multiple ways of doing this, one that seems most elegant and compatible with an MDP prespective on evaluation is to modify all existing rewrite rules.
; These rewrite would say, whenever I transform an object, I shall store its history with it, i.e. what it was before.
; I then have a general rule which matches any object with a history and can revert it.

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

; - Precedence and Decision Making
; Precedence is important in rewrite systems due to conflicts.
; There are at least two types of conflict.
; Both of these occur when an expression is mathed by more than one pattern (pattern + rewrite).
; This could be the same pattern matching multiple times in a single expression:

; square x -> x * x;
; => (square (square x))

; or different patterns matching the expression
; double x -> x + x
; (double (square x))

; The previous two examples were not desctuctive, in the sense that
; performing either of the rewrites did not invalidate the other pattern, and
; hence the order of rewrites has no effect on the fina result.
; There are destructive conflicts however:
; (f (/ x 0) _) -> undefined
; => (square (/ 5 0))

; Rewrite systems typically do not have much machinery in the way of ordering.

; - A DSL which makes it convenient to write these rewrite rules
; We can use macros to define a pure like language for rewriting.

;; Iterators
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
  ^{:doc
    "Iterates through all nodes of an expression depth-first.
     e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
          (+ 3 4 (rand 0 1) (rand 3 4)), +, 3, (rand 0 1), rand, 0, .."}
  [tree zipped-tree])

(defrecord SubtreeIterator
  ^{:doc
    "Iterates through all subtrees of an expression depth-first.
    e.g. (+ 3 4 (rand 0 1) (rand 3 4)) ->
         (+ 3 4 (rand 0 1) (rand 3 4)), (rand 0 1), (rand 3 4)"}
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
  [f init-value pred?]
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
            (repeat-until zip/next
                          (.zipped-tree iterator)
                          #(or (zip/end? %) (zip/branch? %)))]
        (assoc iterator :zipped-tree zip-tree)))))

;; Rule
(defrecord Rule
  ^{:doc "A rule is composed of a predicate, its two operands and conditions"}
  [predicate lhs rhs condition])

(defn rule
  "Rule constructor
   (rule '-> (~(lit 'square) ~(variable 'x)) ~'(* x x) ~'(pos? x)))"
  [predicate lhs rhs condition]
  (->Rule predicate lhs rhs condition))

;; Binding
; The rhs and condition of a rule will have variables which are bound
; when patterns are matches on lhs.
; Option 1: Make rhs and condition be functions which take arguments
; Option 2: Leave them symbolic, do the syntactic replacement and then eval
; I don't want to be calling eval at run-time so I should at least compile them
; into functions ahread of time

(rule '-> `(~(lit 'square) ~(variable 'x)) '(* x x) )
(rule '-> `(~(lit '~*) ~(variable 'x) ~(variable 'y)) '~(* x x))

((fn [x]
  `(~'* ~x ~x)) 3)

;; Pattern Matching
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
    (println "getting here" var input bindings)
    (cond
      (nil? binding-val) (extend-bindings var input bindings)
      
      (= input binding-val) bindings
      
      :else fail)))

; (defn segment-pattern?
;   "is this a segment matching pattern?"
;   [pattern]
;   (and (list? pattern)
;        (.startsWith (name (first pattern)) "?*")))

; TODO


(defn lit
  "make a literal"
  [symbol]
  ['lit symbol])

(defn lit?
  "is it a literal"
  [x]
  (and (vector? x)
       (= (first x) 'lit)))

(defn variable
  "make a variablel"
  [symbol]
  ['variable symbol])

(defn variable?
  "is it a literal"
  [x]
  (and (vector? x)
       (= (first x) 'variable)))

(defn no-var-node-iterator
  "Node iterator factory"
  [exp]
  (NodeIterator. exp (zip/zipper #(and (not (variable? %)) (coll? %))
                      seq (fn [_ c] c) exp)))

(defn pat-match
  "Match pattern against input and bind variables to values.
   Iterate through both simultaneously, when I find a variable in pattern,
   bind that to whatever is at the same spot in exp.

   For a pattern to match, all its variables must match presumably.
  e.g. Pattern = ([lit square] [variable x])
       Input   = (* x x)
       What about 
       (f x? (y? z)) (f hello d)
  "
  [pattern exp]
  (loop [pattern-itr (no-var-node-iterator pattern)
         exp-itr (no-var-node-iterator exp)
         bindings {}]
    (println "pattern is" (realise pattern-itr) "exp" (realise exp-itr)
              "bindings" bindings)
    (cond
      (= bindings fail)
      fail

      (and (end? pattern-itr) (end? exp-itr))
      bindings

      ;Equal so no binding but continue
      (= (realise pattern-itr) (realise exp-itr)) 
      (recur (step pattern-itr) (step exp-itr) bindings)

      (variable? (realise pattern-itr))
      (recur (step pattern-itr) (step exp-itr)
             (match-variable (realise pattern-itr) (realise exp-itr) bindings))

      (and (list? (realise pattern-itr))
           (list? (realise exp-itr)))
      (recur (step pattern-itr) (step exp-itr) bindings)

      :else ; literal in both do not match
      fail)))

(defn pat-rewrite
  "Determine whether a pattern matches, if so, rewrite accordingly.
   Else return nil"
  [exp rule]
  (if-let [bindings (pat-match (pattern rule) exp)]
           (bind-bindings-to-rewrite)
           nil))

;; Transformers
(defn apply-first-transform
  "Do the first transform that is applicable from an ordered set of rules
   else nil"
  [exp rules]
  (loop-until (partial pat-rewrite exp) rules))

(defn eager-transformer
  "This is an eager transformer.
   It matches the rules in order and applies first match"
  [rules exp]
  (loop [iterator (subtree-iterator exp)]
    (end? iterator) ; recurse until no rules matched
    nil

    ; try first possible rewrite on this subtree, otherwise move to next
    (if-let [exp (apply-first-transform (realise iterator) rules)]
      exp
      (recur (step iterator)))))

(defn rewrite
  "Rewrite an expression using a relation"
  [exp transform]
  (repeat-until transform exp nil?))

;; Parse DSL - more convenient notation for writing rules
; We create pure-lang like DSL for more concise rule writing
(defn split-coll-at
  "Split vector along 'at'"
  [coll at]
  (loop [split-colls [] coll coll]
    (println "coll is" coll)
    (let [index (.indexOf coll at)]
      (if (= index -1)
          (conj split-colls coll)
          (recur (conj split-colls (subvec coll 0 index)) 
                 (subvec coll (inc index)))))))

(comment

  ; (defrule primitive-f
  ;   "evaluate primitive functions"
  ;   f x1 .. xn -> (f x1 .. x2) where (and (function? f)
  ;                                         (primitive? f)
  ;                                         (evaluated? xi)))

  ; (def a-rule '(-> (square x) (* x x) (and (pos? x))))
  ; (def primitive-apply-rule
  ;   '(-> (f x1 .. xn) ~(f x1 .. x2) (and (function? f)
  ;                                        (primitive? f)
  ;                                        (evaluated? xi))))

  ; (def rand-rule
  ;   '(-> (rand x y) (->interval-abo x y)))

  ; (def convex-hull-rule
  ;   '(⊑ (or x y) ~(convex-hull x y) (abo? x)))

  ; (def rules [a-rule primitive-apply-rule rand-rule convex-hull-rule])

  ;
  (def term '(+ 3 4 (rand 0 1) (rand 3 4)))
  (def a-rule `(~'-> (~(lit 'square) ~(variable 'x)) ~'(* x x) ~'(pos? x)))
  (def transformer (partial eager-transformer [a-rule]))
  
  ; (def evaluated-term (rewrite term eager-transformer)))