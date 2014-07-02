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

;; =================
;; Contexts Protocol

(defprotocol Context
  ^{:doc
    "Contexts provide an interface for traversing over sub-structures of an object
     A pattern may be applicable to a complex object on several levels
     E.g. a pattern (?a ?b) matched against ((1 1)(2 2))
     could match ?a = 1, ?b = 1 or ?a = (1 1) ?b = (2 2), etc"}
  (context-itr [context obj]))

(defprotocol BoundVarContext
  ^{:doc "Context on bound-variables - no longer sur eof hte point of this"}
  (matches? [context obj]))

;; A type for s-expressions
(defrecord ExprContext
  ^{:doc "A context for s-expressions which returns an iterator over all terms"}
  [term-itr bound-vars-ok?])

(extend-type ExprContext Context
  (context-itr [context obj]
    ((.term-itr context) obj)))

(extend-type ExprContext BoundVarContext
  (matches? [context bindings]
    ((.bound-vars-ok? context) bindings)))

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
