(ns ^{:doc "Domain specific language for pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.dsl
  (:require [veneer.pattern.rule :refer [->Rule]]
            [veneer.pattern.match :refer [->CorePattern]]
            [veneer.pattern.match-macros :refer [match-fn]]
            #+clj [veneer.pattern.match-macros :refer [match-fn]]
            [clozen.debug :as debug]
            [clozen.helpers :refer [in?]]
            [clozen.zip :as clzn.zip]
            [clozen.iterator :as clzn.itr :refer [realise step]]
            [clojure.zip :as zip]
            [clojure.walk :refer [postwalk]])
  #+cljs (:require-macros [veneer.pattern.match-macros :refer [match-fn]])
  )

; Parsed DSL - more convenient notation for writing rules
; We create pure-lang like DSL for more concise rule writing
(defn dsl-lhs-expand [kw-to-vars x]
  (cond
    (list? x) `([~@x] :seq)
    (and (symbol? x) (not (in? (vals kw-to-vars) x))) x
    :else x))

(defn lhs-to-corepattern [lhs kw-to-vars]
  "Convert dsl pattern to a clojure.core.match type pattern
   Start at the leaves and replace any convert lists into seq form"
  `(->CorePattern
    (match-fn
      ~(postwalk (partial dsl-lhs-expand kw-to-vars) lhs) ~kw-to-vars
      :else nil)))

(defn forced-var?
  "is x a variable (a symbol starting with ?"
  [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn special-form? [x]
  (in? '[& fn let if cond let* def defn] x))

(defn extract-pattern-vars
  "Do a recursive walk through a term and extract the variables"
  [term]
  (if (coll? term)
      (loop [itr (clzn.itr/node-itr term) vars []]
        (cond
          (clzn.itr/end? itr) vars
          (symbol? (realise itr))   ;If its a symbol and..
          (cond
            (forced-var? (realise itr))   ;..if its a forced-var add-to-list
            (recur (step itr) (conj vars (realise itr)))

            ; or its not special and head of the list, add to list
            (and (not (special-form? (realise itr)))
                 (not (zero? (clzn.zip/zip-loc-pos (:zipped-tree itr))))
                 (not= 'quote (first (zip/leftmost (:zipped-tree itr)))))
            (recur (step itr) (conj vars (realise itr)))

            :else
            (recur (step itr) vars))
          :else
          (recur (step itr) vars)))
      ; If its just a single term then its the only variable
      [term]))



(comment
  (require '[fipp.edn :refer (pprint) :rename {pprint fipp}])

  (require '[veneer.pattern.match :refer :all]
           '[veneer.pattern.rule :refer :all])

  (def primitive-apply-rule
    "This rule applies a primitive function"
    (->Rule '->
          (->CorePattern (match-fn
                           ([f & args] :seq) {:f f :args args}
                           :else nil))
          (fn [{f :f args :args}]
            (apply (primitive f) args))
          (->ExprContext
            itr/subtree-leaves-first-itr)
            (fn [{f :f args :args}]
              (and (primitive? f)
                   (evaluated? args))))
          nil)

  (lhs-to-corepattern '((fn [& args] body) & params))

  ;; Normal Application Rules
  (require '[fipp.edn :refer (pprint) :rename {pprint fipp}])

  (defrule primitive-apply-rule
        "Apply primitive functions"
        (-> (?f & args) (apply ?f args) :when (and (primitive? f)
                                                 (evaluated? args))))
  (defn primitive? [x] true)
  (def evaluated? primitive?)
  (def expanded
    (macroexpand
    '(defrule primitive-apply-rule
     "Apply primitive functions"
     (-> (?f & args) (apply ?f args) :when (and (primitive? ?f)
                                             (evaluated? args))))))

  (def expanded2
    (macroexpand
    '(defrule eval-primitives
  "Eval primitive functions"
  (-> x (primitive x) :when (do (println "x is" x) (primitive-symbol? x))))))

  (fipp expanded2)

  (def
   primitive-apply-rule
   "Apply primitive functions"
   (rule
    rel
    (->CorePattern
     (match-fn ([?f & args] :seq) veneer.pattern.dsl/kw-to-vars :else nil))
    (fn
     [{args :args, & :&, ?f :?f}]
     ((apply f args) :when (and (primitive? f) (evaluated? args))))))

)
