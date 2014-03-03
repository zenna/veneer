(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:import [clozen.iterator NodeIterator SubtreeIterator])
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

;; TODO
; Features
; DSL, need more convenient notation. Some combination of function and macros
; take spec e.g
;     f x1 .. xn -> (f x1 .. x2) where (and (function? f)
;                                           (primitive? f)
;                                           (evaluated? xi)))

; recognise f is the literal, xns are variables, make rewrite function with variables bound, and condition with variables bound.
; Encode all evaluation rules as rewrite rules
;  

; Code smell
; Repeat until / repeat before until need better names
; I have this lit? code, its not being used, decide what is correct to do there
; no-var-node-iterator seems a bit ad hoc, what is the general problem and solution
; is this X'ing out of matched patterns the right thing to do?
; Do i have the write logic in my pattern matcher
; is seq? the right predicate for when to go inside
; Can I add types to my rules?

; The evaluation of the program is a kind of problem, but what kind?

; At a high level when we evaluate a program approximately, we have to make choices about which approximatiosn we take.
; The choices we make may have consequences in the future, and we may have to revise our choices.
; This motivations two perspectives on the kind of problem this is, a) a planning problem b) a graph search problem c) an local optimisation.

; The main properties of the problem are this
; - The interpreter is given a valid lisp program p.
; - In order to evaluate this program the interpreter has a set of transformations it can apply to the program.
; - some of these transformations may may be parameterised, and hence in order to be applied the interpreter much assign appropriate parameter values
; - there is no normal form, hence no single goal state. Many states are goals, all of which must app
; - For some transformations there is no important choice to make, since the transformation is not an approximating transformation.
; - varying transformations may take varying amounts of resources
; - they may also result in better or worse approximatiosn
; - the quality or cost of every state should be some function of the fidelity of the approximatinon
; - its not clear whehter its meaningful to assign costs to non-goal states
; - what is a goal state anyway?

;; TODO
; Make this work on the mean and planning example
; Devise and code inverse graphics example
; Refacator out iterator and graph to clozen or elsewhere - DONE
; 

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
    ; (println "Match Variable" var input bindings)
    (cond
      (nil? binding-val) (extend-bindings var input bindings)
      
      (= input binding-val) bindings
      
      :else fail)))

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
  "make a variable"
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
    ; (println "Pattern is:" (realise pattern-itr) " ,Exp:" (realise exp-itr)
    ;           ", Bindings:" bindings)
    (cond
      (= bindings fail) ; Possibly due to variable matching two different vals
      fail              ; As determined by match-variable in previous iter

      ;; The only time we succeed is if we reach the end of the pattern & exp
      ;; and all variables are matched, and seen no conflicts along the way
      (and (end? pattern-itr) (end? exp-itr))
      bindings

      (or (end? pattern-itr) (end? exp-itr)) ; Reached end and not all matched
      fail

      ;Equal so no binding but continue
      (= (realise pattern-itr) (realise exp-itr)) 
      (recur (step pattern-itr) (step exp-itr) bindings)

      ;; If the matching is successful and the matched item is a list, we can
      ;; skip over that by 
      (variable? (realise pattern-itr))
      (let [new-b (match-variable (realise pattern-itr)
                                  (realise exp-itr) bindings)]
        (recur (step pattern-itr)
               (step (if (not= new-b fail) (update exp-itr 'X) exp-itr))
               new-b))

      (and (seq? (realise pattern-itr))
           (seq? (realise exp-itr)))
      (recur (step pattern-itr) (step exp-itr) bindings)

      :else ; literal in both do not match
      fail)))

;; Rule
(defrecord Rule
  ^{:doc "A rule is composed of a predicate, its two operands and conditions"}
  [predicate lhs rhs condition])

(defn rule
  "Rule constructor
   (rule '-> (~(lit 'square) ~(variable 'x)) ~'(* x x) ~'(pos? x)))"
  [predicate lhs rhs condition]
  (->Rule predicate lhs rhs condition))

(defn pat-rewrite
  "Determine whether a pattern matches, if so, rewrite accordingly.
   Else return nil"
  [exp rule]
  (if-let [bindings (pat-match (:lhs rule) exp)]
    (if ((:condition rule) bindings) ;if we match and conditions pass
        ((:rhs rule) bindings)
        nil)
    nil))

;; Transformers
(defn apply-first-transform
  "Do the first transform that is applicable from an ordered set of rules
   else nil"
  [exp rules]
  (clzn/loop-until-fn (partial pat-rewrite exp) rules))

(defn eager-transformer
  "This is an eager transformer.
   It matches the rules in order and applies first match"
  [rules exp]
  ; (println "getting in eager transformer")
  (loop [iterator (subtree-iterator exp)]
    (if (end? iterator) ; recurse until no rules matched
        nil
        ; try first possible rewrite on this subtree, otherwise move to next
        (if-let [exp (apply-first-transform (realise iterator) rules)]
          ; Rewrite part of expression iterator currently pointing to,
          ; then returh entire root
          (root (update iterator exp)) 
          (recur (step iterator))))))

(defn rewrite
  "Rewrite an expression using a relation"
  [exp transform]
  (clzn/repeat-before-until #(debug/dbg (transform %)) exp nil?))

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

(def primitives {'+ + '* * '- +})

(defn primitive
  "Get evaluable function from symbol"
  [symb]
  (if-let [f (primitives symb)]
    f
    (eval symb)))

(defn primitive?
  "Is the symbol a primitive?"
  [symb]
  (clzn/nil-to-false (primitives symb)))

(defn evaluated?
  "Is this expression fully evaluated? Check by seeing if any more
   rewrite rules can be applied"
  [exp]
  (number? exp)) ; FIXME this is wrong
  ; (if (clzn/loop-until-fn #(pat-match (:lhs %) exp) rules)
  ;   true
  ;   false))

(comment
  (def a-rule
    (rule '->
          `(~'square ~(variable 'x)) ; lhs
          (fn [{x (variable 'x)}]           ; rhs
            `(~'* ~x ~x))
          (fn [_]           ; condition
            true)))

  (def mul-rule
    (rule '->
          `(~'* ~(variable 'x) ~(variable 'y)) ; lhs
          (fn [{x (variable 'x) y (variable 'y)}]           ; rhs
            (* x y))
          (fn [{x (variable 'x) y (variable 'y)}]           ; condition
            (and (number? x) (number? y)))))

  ;; The problem here is that I want to tell if there are any more transforms
  ;; To do, so the condition is dependent on what I have evaluated already
  (def primitive-apply-rule
    (rule '->
          `(~(variable 'f) ~(variable 'x) ~(variable 'y))
          (fn [{f (variable 'f) x (variable 'x) y (variable 'y)}]
            ((primitive f) x y))
          (fn [{f (variable 'f) x (variable 'x) y (variable 'y)}]
            (and (primitive? f)
                 (evaluated? x)
                 (evaluated? y)))))

  (def a-exp '(+ (square (square 3)) 2))
  (def b-exp '(* 2 9))
  (def transformer (partial eager-transformer [a-rule primitive-apply-rule]))
  (rewrite a-exp transformer)
  (def result (rewrite a-exp transformer)))