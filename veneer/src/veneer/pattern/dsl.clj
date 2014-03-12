(ns ^{:doc "Domain specific language for pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.dsl
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:require [clojure.zip :as zip]))


;; Parse DSL - more convenient notation for writing rules
; We create pure-lang like DSL for more concise rule writing

;; Normal Application Rules
(defrule primitive-apply-rule
  "Apply primitive functions"
  (-> (f & args) (apply f args) :when (and (primitive? f)
                                           (evaluated? args))))
(defrule compound-f-sub-rule
  "Substitute in a compound function"
  (-> (f & args) `(~(lookup-compound f) ~@args) :when (compound? f)))

(defrule sub-vars-rule
  "A variable substitution rule"
  (-> ((fn [& args] body) & params) (rewrite )

(defrule if-rule
  "Evaluated if rule"
  (-> [(if true branch alternative)
       (if false consequent branch)] branch))

(defrule def-rule
  "Equivalent to defn macro"
  (-> (defn name docstring args body) `(def (fn args) body)))

(defrule defn-rule
  "Equivalent to defn macro"
  (-> (defn name docstring args body) `(def (fn args) body)))

;; Abstract Operators
; Random Primitives
(defrule
  "Interval abstraction of uniform real"
  (⊑ (rand) (interval-abo 0 1)))

(defrule
  "Interval abstraction of uniform real"
  (⊑ (rand-int x y) (interval-abo 0 1)))

(defrule
  "Symbolic abstraction of gaussian"
  (-> (bernoulli) (bernoulli 0.5)))

(defrule
  "Symbolic abstraction of gaussian"
  (⊑ (bernoulli p) (interval-abo 0 1)))

(defrule
  "Symbolic abstraction of gaussian"
  (⊑ (unif-real) (interval-abo 0 1)))


;; Conversion between abstract domains
(defrule
  "We can cover a convex polyhedra with boxes"
  (⊑ x (cover %n-boxes x) :where (convex-polytope? x)))

(defrule
  "We can cover a convex polyhedra with boxes"
  (⊑ x (cover %n-boxes x) :where (convex-polytope? x)))

;; Primitive Operators on abstract domains
(defrule +-interval-abo
  "Add two intervals"
  (⊑ (+ & args) :let [[int-args others] (separate int-abo? args)]
     [(apply add-intervals int-args) :when (empty? others)
      (+ @others (apply add-intervals int-args)) :when (seq? int-args)]))

; Add intervals to real values

; Add convex polyhedra together


;; Under approximations

;; Samples
(defrule
  "Interval abstraction of uniform real"
  (~ (rand) (rand)))

(defmacro defrule
  [name docstring rel]
  ;; I have to convert lhs into appropriate CorePatternFormat
  ;; I have to extract the parameters
  (let [])

  `(def name docstring

(defn assignment-rules
  ""
  [lhs rhs]

(defn is-symbol?
  [symbol]
  (or (vari? symbol) (pos-in-list symbol 0)))

(rule -> (?f &args) (~(lookup-compound f) &args) :when (compound? f))
(-> (?f &args) (~(lookup-compound f) &args) (compound? f))
(rule (?f &args) [(~(lookup-compound f) &args) :when ] (compound? f))

(defmacro rule-dsl
  "For our rule syntaax "
  [& args]
  (let [[rel lhs rhs condition] args]
    (postwalk 

(defrule
  variable-sub
  -> '((fn [&args] body) &params)
     (rewrite body (assignment-rules args params))
     (count= args params)