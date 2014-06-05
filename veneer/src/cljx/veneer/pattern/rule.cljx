(ns ^{:doc "Rewriting"
      :author "Zenna Tavares"}
  veneer.pattern.rule
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer [update root end? realise step]]
            [veneer.pattern.match :refer [pat-match]]))

(defrecord Rule
  ^{:doc " lhs - left pattern
           rhs - right pattern
           rel - how left and right pattern are related, e.g. rewrite ->
           pre-context - context on lhs
           post-context - context on rhs"}
  [rel lhs rhs pre-context post-context])

;; Contexts
; Some contexts may be iterable, some may return a vector in parallel of substructures - what do they all have in common, some type presumably.
(defprotocol Context
  (context-itr [context obj]))

(defprotocol BoundVarContext
  (matches? [context obj]))

;; A type for s-expressions
(defrecord ExprContext [term-itr bound-vars-ok?])

(extend-type ExprContext Context
  (context-itr [context obj]
    ((.term-itr context) obj)))

(extend-type ExprContext BoundVarContext
  (matches? [context bindings]
    ((.bound-vars-ok? context) bindings)))

(defn rule
  "Rule constructor"
  [predicate lhs rhs pre-context post-context]
  (->Rule predicate lhs rhs pre-context post-context))

(defmulti pre-context-check
  (fn [pre-context bindings] (satisfies? BoundVarContext pre-context)))

(defmethod pre-context-check true
  [pre-context bindings]
  (matches? pre-context bindings))

(defmethod pre-context-check false
  [pre-context bindings]
  true)

(defn post-context-checks
  [& _]
  true)

(defn pat-rewrite
  "Determine whether a pattern matches, if so, rewrite accordingly.
   Else return nil"
  [exp rule]
  ; (println "lhs is" (:lhs rule) "exp is" exp)
  (if-let [bindings (pat-match (:lhs rule) exp)]
    (if (and
          (pre-context-check (:pre-context rule) bindings)
          (post-context-checks (:post-context rule) bindings))
        ((:rhs rule) bindings)
        nil)
    nil))