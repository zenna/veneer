(ns ^{:doc "Rewriting"
      :author "Zenna Tavares"}
  veneer.pattern.rewrite
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:require [veneer.pattern.match :refer :all]))

;; It's a bit confusing about how these ideas fit together
;; On the one hand we have a pattern matching library that seems to have
;; Most if not all of the functionality I need, and is most likely much faster
;; Than what I have done so I should use it

;; What's confusing is where the application of different rules comes into play
;; In rewriting I have a big complex expression which is my whole program
;; And my goal is to find patterns and rewrite those terms

;; Possibility 1: my patterns are written to match against entire expression
; e.g. (_ (* x y) _) which will match (* x y) no matter where it is located

;; Possibility 2: my patterns are written just to match against a term and I need to enumerate terms
; There are two parts to this system.
; The first is a set of sentences, for instance (+ x y) = (+ y x), (square x) -> (* x x), x ⊑ (convex-hull x).
; The latter is a transformer (equally an interpeter, evaluator or rewriter)
; The combination of the relations together with the transformer define a graph.
; Each node of this graph is a term and nodes are connected by the transformers interpretation of the relations.

; We may represent abstraction rules as, for example, abstraction relations, e.g. (or x y z) ⊑ (convex-hull x y z).
; A sound transformer shall then have to recognise that it can replace any term with its approximation.
; An unsound transformer might even do the opposite.
; There may be no normal form, instead there may be a variety of normal forms.
(defrecord Rule
  ^{:doc "A rule is composed of a predicate, its two operands and conditions"}
  [predicate lhs rhs itr applicable? condition])

; A rule really has a 
; predicate - defines relationship between its operands, which are patterns, e.g x = y, x approximates y, x can be rewritten to y.
; A pattern has a dual perspective, it acts as a predicate when when given an object will tell you whether it matches, in this sense it partitions the space of all objects.
; But it also has a generative component: it partially specifies what it requires to - (+ x _) could generate (+ x 3) or (+ x 10) anything it generates would of course match.
; The type of a pattern can be thought of as Maybe Map
; if it doesn't match it returns nil, otherwise it will return variable bindings, which of course could be an empty list.
; From a pure perspective we can think of a context as a predicate, telling us whether a rule is applicable in;
; In reality we embed the context a generative or iterative component that allows it to
; (defrecord NewRule
;   ^{:doc "A rule is composed of a relation, its two operands a context"}
;   [rel lhs rhs pre-context post-context])

; (defprotocol Context
;   (can-apply? [context rule obj]) ; Can this rule be applied in this context
;   (next [])

; (defn new-rule
;   "New Rule Constructor"
;   [rel lhs rhs pre-context post-context]
;   (->NewRule rel lhs rhs context))

; (defn add-precontext)

; (defn add-postcontext)

; Context - This adds extra information about where a pattern is applicable.
; - For evaluation of lisp programs, most contexts will be lists, 
; pattern - acts as a predicate which matches an object and binds variables
; the transform - 
; pre-condition, 
; post-condition.

(defn rule
  "Rule constructor
   (rule '-> (~(lit 'square) ~(variable 'x)) ~'(* x x) ~'(pos? x)))"
  ([predicate lhs rhs iterator applicable? condition]
    (->Rule predicate lhs rhs iterator applicable? condition))
  ([predicate lhs rhs applicable? iterator]
    (->Rule predicate lhs rhs applicable? iterator (fn [& _] true))))

(defn pat-rewrite
  "Determine whether a pattern matches, if so, rewrite accordingly.
   Else return nil"
  [exp rule]
  ; (println "lhs is" (:lhs rule) "exp is" exp)
  (if-let [bindings (pat-match (:lhs rule) exp)]
    (if ((:condition rule) bindings) ;if we match and conditions pass
        ((:rhs rule) bindings)
        nil)
    nil))

; (defn apply-rule
;   "Apply this rule to all the nested expressions and rewrite if match"
;   [exp rule]
;   (if (can-apply? rule exp)
;       (next-context rule exp)
;       (loop [itr ((.itr rule) exp)]
;         (if (end? itr) nil
;             (if-let [exp (pat-rewrite (realise itr) rule)]
;               (root (update itr exp))
;               (recur (step itr)))))
;       nil))

(defn apply-rule
  "Apply this rule to all the nested expressions and rewrite if match"
  [exp rule]
  (if ((.applicable? rule) exp)
      (loop [itr ((.itr rule) exp)]
        (if (end? itr) nil
            (if-let [exp (pat-rewrite (realise itr) rule)]
              (root (update itr exp))
              (recur (step itr)))))
      nil))

(defn eager-transformer
  "This is an eager transformer.
   It matches the rules in order and applies first match"
  [rules exp]
  (clzn/loop-until-fn (partial apply-rule exp) rules))

(defn rewrite
  "Rewrite an expression using a relation"
  [exp transform]
  (clzn/repeat-before-until #(debug/dbg (transform %)) exp nil?))