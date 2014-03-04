(ns ^{:doc "Domain specific language for pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.dsl
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:import [clozen.iterator NodeIterator SubtreeIterator])
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