(ns ^{:doc "Domain specific language for pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.dsl
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:require [clojure.zip :as zip]))


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

; (def primitive-apply-rule
;   "This rule applies a primitive function"
;   (rule '->
;         (->CorePattern (match-fn x
;                          ([f & args] :seq) {:f f :args args}
;                          :else nil))
;         (fn [{f :f args :args}]
;           (apply (primitive f) args))
;         (->ExprContext
;           itr/subtree-leaves-first-itr
;           (fn [{f :f args :args}]
;             (and (primitive? f)
;                  (evaluated? args))))
;         nil))

(defrule primitive-apply-rule
  "Apply primitive functions"
  (-> (f & args) (apply f args) :when (and (primitive? f)
                                          (evaluated? args))))

; (def compound-f-sub-rule
;   "Substitute in a compound function"
;   (rule '->
;         (->CorePattern (match-fn x
;                          ([f & args] :seq) {:f f :args args}
;                          :else nil))
;         (fn [{f :f args :args}]
;           `(~(lookup-compound f) ~@args))
;         (->ExprContext
;           itr/subtree-itr
;           (fn [{f :f}]
;             (compound? f)))
;         nil))

(defrule compound-f-sub-rule
  "Substitute in a compound function"
  (-> (f & args) `(~(lookup-compound f) ~@args) when (compound? f)))

; (def variable-sub-rule
;   "Substitute in variables"
;   (rule 
;   '->
;   (->CorePattern (match-fn x
;                    ([(['fn [& args] body] :seq) & params] :seq)
;                      {:args args :body body :params params}
;                    :else nil))
;   (fn [{args :args body :body params :params}]
;     (postwalk-replace (zipmap args params) body))
;   (->ExprContext
;     itr/subtree-itr
;     (fn [_ &]
;       true))
;   nil))

(defrule
  "A variable substitution rule"
  (-> ((fn [& args] body) &params) (postwalk-replace (zipmap args params) body)

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