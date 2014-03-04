(ns ^{:doc "Rewriting"
      :author "Zenna Tavares"}
  veneer.pattern.rewrite)

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

; Code smell


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