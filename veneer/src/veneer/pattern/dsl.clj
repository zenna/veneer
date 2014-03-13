(ns ^{:doc "Domain specific language for pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.dsl
  (:require [veneer.pattern.rule :refer [rule]])
  (:require [clozen.debug :as debug]
            [clozen.helpers :refer [in?]]
            [clozen.zip :as clzn.zip]
            [clozen.iterator :as clzn.itr :refer [realise step]])
  (:require [clojure.zip :as zip]
            [clojure.walk :refer [postwalk]])
  (:require [fipp.edn :refer (pprint) :rename {pprint fipp}]))

; Parse DSL - more convenient notation for writing rules
; We create pure-lang like DSL for more concise rule writing
(defn dsl-lhs-expand [kw-to-vars x]
  (cond
    (list? x) `([~@x] :seq)
    (and (symbol? x) (not (in? (vals kw-to-vars) x))) x
    :else x))

(defn lhs-to-corepattern [lhs kw-to-vars]
  `(~'->CorePattern
    (~'match-fn ~'x
      ~(postwalk (partial dsl-lhs-expand kw-to-vars) lhs) ~kw-to-vars
      :else nil)))

(defn forced-var?
  "is x a variable (a symbol starting with ?"
  [x]
  (and (symbol? x) (.startsWith (name x) "?")))

(defn extract-pattern-vars
  "Do a recursive walk through a pattern and extract the variables"
  [term]
  (if (coll? term)
      (loop [itr (clzn.itr/node-itr term) vars []]
      (debug/dbg (realise itr))
      (cond
        (clzn.itr/end? itr) vars
        (debug/dbg (symbol? (realise itr)))
        (cond
          (forced-var? (realise itr))
          (recur (step itr) (conj vars (realise itr)))

          (and (not= '& (realise itr)) (debug/dbg (not (zero? (clzn.zip/zip-loc-pos (:zipped-tree itr))))))
          (recur (step itr) (conj vars (realise itr)))

          :else
          (recur (step itr) vars))
        :else
        (recur (step itr) vars)))
      [term]))

;; TODO
;- Suitable context and iterators
;- Conditionals :when
;- Fix &
; - Parameters
; - Ors
(defmacro defrule
  "## Variables 
   We follow a pure-lang like convention:
   If there is a symbol in the pattern, we consider it a variable
   -- if it is not the first of a list Or it is prefixed with ?
   We consider it a literal if it is:
   -- The first in the list of quoted

   ## Context
   We'll use the node iterator constrained to type of lhs
   "
  [name docstring a-rule]
  {:pre [(list? a-rule)]
   :post (fipp (macroexpand %))}
  (let [[rel lhs rhs & whens] a-rule
        pat-vars (extract-pattern-vars lhs)
        kw-to-vars (zipmap (map keyword pat-vars) pat-vars)
        vars-to-kw (zipmap pat-vars (map keyword pat-vars))]
  `(def ~name ~docstring
     (~'rule
      (quote ~rel)
      ~(lhs-to-corepattern lhs kw-to-vars)
      (~'fn [~vars-to-kw] ~rhs)
      (~'->ExprContext
        ~(if (coll? lhs)
             `(~'fn [~'exp]
               (clzn.itr/add-itr-constraint (clzn.itr/node-itr ~'exp) 
                                       #(coll? (clzn.itr/realise %))))
             'itr/node-itr)
        ~(if whens
            `(~'fn [~vars-to-kw] ~(nth whens 1))
            '(fn [_ &] true)))
        nil))))

(comment
  (def primitive-apply-rule
    "This rule applies a primitive function"
    (rule '->
          (->CorePattern (match-fn x
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
     (match-fn x ([?f & args] :seq) veneer.pattern.dsl/kw-to-vars :else nil))
    (fn
     [{args :args, & :&, ?f :?f}]
     ((apply f args) :when (and (primitive? f) (evaluated? args))))))

)