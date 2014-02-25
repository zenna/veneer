(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match)

- History: Rewrites vs Equations
When evaluating a program, in particular when we have to make several choices
regarding which abstractions to use and to what precision, there may be
times where we want to undo.
We may wish to revert our decisions and make better ones in light of new evidence.
Rewriting a program is inherently destructive, information is lost along the
way.  In order to be able to backtrack there seem to be two viable alternatives.
1. The agent stores his history, and other information as he sees fit.
2. We do not do rewriting in the stricted sense of the term, instead
the program is one point in a discrete state space, adjoined by transitions
made possible through rewrites.

Practical difference
Rewrites are not directional, they are equations.
It is no longer pattern/rewrite

e.g.
square x = x * x is bidirectional.
sqrt x = ~(sqrt [x]) when (positive? x)

mul x y = ~(* x y)

If I have 4, I could convert it to (mul 2 2), but multiplication is a one
way function.

Pros of 1.
We keep a standard rewrite system, which can be used for other purposes.

- Performance


- Precedence and Decision Making
Precedence is important in rewrite systems due to conflicts.
There are at least two types of conflict.
Both of these occur when an expression is mathed by more than one pattern (pattern + rewrite).
This could be the same pattern matching multiple times in a single expression:

square x -> x * x;
=> (square (square x))

or different patterns matching the expression
double x -> x + x
(double (square x))

The previous two examples were not desctuctive, in the sense that
performing either of the rewrites did not invalidate the other pattern, and
hence the order of rewrites has no effect on the fina result.
There are destructive conflicts however:
(f (/ x 0) _) -> undefined
=> (square (/ 5 0))

Rewrite systems typically do not have much machinery in the way of ordering.

- A DSL which makes it convenient to write these rewrite rules
We can use macros to define a pure like language for rewriting.

;; Parse DSL
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


; (defmacro defrule
;   [docstring & args]
;   {:pre [in? args '->]}

; ;; Rule Type
; (defrecord Rule
;   "A

;    An iterator - (iterate exp) returns the next element.
;    How to do this in a functional way, e.g. if I want to do
;    bfs.
;    Returns nil when nothing left to iterate"
;   [pattern rewrite condition iterate]

; (defn rule
;   "Create a rule, e.g. '((square x) -> x * x)"
;   [rule]

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

(comment
  (->Rule '(square x) '(* x x) )
  (defrule square "Square" x -> x * x)
  (pat-match '(square ?x) '(+ (square 10) (square 20)))

  (defrule primitive-f
    "evaluate primitive functions"
    f x1 .. xn -> (f x1 .. x2) where (and (function? f)
                                          (primitive? f)
                                          (evaluated? xi))))