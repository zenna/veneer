(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match
  (:require [clozen.debug :as debug]
            [clozen.helpers :as clzn]
            [clozen.iterator :refer :all])
  (:require [clojure.core.match :refer [clj-form match]])
  (:import [clozen.iterator NodeItr])
  (:require [clojure.zip :as zip]))

; Repeat until / repeat before until need better names - now in clozen
; I have this lit? code, its not being used, decide what is correct to do there
; no-var-node-iterator seems a bit ad hoc, what is the general problem and solution
; is this X'ing out of matched patterns the right thing to do?
; Do i have the write logic in my pattern matcher
; is seq? the right predicate for when to go inside
; Can I add types to my rules?
; Make this work on the mean and planning example
; Devise and code inverse graphics example
; Need to differentiate between conditions I Want for debugging and other conditions, e.g. count= for arglist and paramlist should not need to be tested, unless your program is incorrect.

(defprotocol PatternMatcher
  "A pattern matcher must just provide a pat-match function"
  (pat-match [pattern exp]))

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

(defn extend-bindings
  "Adds a var binding to binding map"
  [var input bindings]
  (assoc bindings var input))

(defn match-variable
  "Does variable match input?"
  [var input bindings]
  (let [binding-val (get-binding var bindings)]
    ; (println "Match Variable" var input bindings)
    (cond
      (nil? binding-val) (extend-bindings var input bindings)
      
      (= input binding-val) bindings
      
      :else fail)))

(defn lit
  "make a literal"
  [symbol]
  ['lit symbol])

(defn lit?
  "is it a literal"
  [x]
  (and (vector? x)
       (= (first x) 'lit)))

(defn variable
  "make a variable"
  [symbol]
  ['variable symbol])

(defn variable?
  "is it a literal"
  [x]
  (and (vector? x)
       (= (first x) 'variable)))

(defn wildcard
  "make a wild card"
  [symbol]
  ['wildcard symbol])

(defn wildcard?
  "is it a wildcard"
  [x]
  (and (vector? x)
       (= (first x) 'wildcard)))

(defn no-var-node-iterator
  "Node iterator factory"
  [exp]
  (NodeItr. exp (zip/zipper #(and (not (variable? %)) (coll? %))
                      seq (fn [_ c] c) exp)))

(defn linear-pat-match
  "Match pattern against input and bind variables to values.
   Iterate through both simultaneously, when I find a variable in pattern,
   bind that to whatever is at the same spot in exp.

   For a pattern to match, all its variables must match presumably.
  e.g. Pattern = ([lit square] [variable x])
       Input   = (* x x)
       What about 
       (f x? (y? z)) (f hello d)
  "
  [pattern exp]
  (loop [pattern-itr (no-var-node-iterator pattern)
         exp-itr (no-var-node-iterator exp)
         bindings {}]
    ; (println "Pattern is:" (realise pattern-itr) " ,Exp:" (realise exp-itr)
    ;           ", Bindings:" bindings)
    (cond
      (= bindings fail) ; Possibly due to variable matching two different vals
      fail              ; As determined by match-variable in previous iter

      ;; The only time we succeed is if we reach the end of the pattern & exp
      ;; and all variables are matched, and seen no conflicts along the way
      (and (end? pattern-itr) (end? exp-itr))
      bindings

      (or (end? pattern-itr) (end? exp-itr)) ; Reached end and not all matched
      fail

      ;Equal so no binding but continue
      (= (realise pattern-itr) (realise exp-itr)) 
      (recur (step pattern-itr) (step exp-itr) bindings)

      ;; If the matching is successful and the matched item is a list, we can
      ;; skip over that by 
      (variable? (realise pattern-itr))
      (let [new-b (match-variable (realise pattern-itr)
                                  (realise exp-itr) bindings)]
        (recur (step pattern-itr)
               (step (if (not= new-b fail) (update exp-itr 'X) exp-itr))
               new-b))

      (and (seq? (realise pattern-itr))
           (seq? (realise exp-itr)))
      (recur (step pattern-itr) (step exp-itr) bindings)

      :else ; literal in both do not match
      fail)))

(defrecord LinearPattern
  ^{:doc "Checks the pattern linear ala Norvig - lisp"}
  [pattern])

(defrecord CorePattern
  ^{:doc "This uses clojure.core.match by Nolan"}
  [pattern])

(extend-protocol PatternMatcher
  LinearPattern
  (pat-match [pattern exp] (linear-pat-match (:pattern pattern) exp))
  CorePattern
  (pat-match [pattern exp] ((:pattern pattern) exp)))

(defmacro match-fn
  "Pattern match a row of occurrences. Take a vector of occurrences, vars.
  Clause question-answer syntax is like `cond`. Questions must be
  wrapped in a vector, with same arity as vars. Last question can be :else,
  which expands to a row of wildcards.
  
  Example:
  (let [x 1
        y 2]
    (match [x y 3]
      [1 2 3] :answer1
      :else :default-answer))"
  [& clauses]
  (let [vars (gensym "ok")
        [vars clauses]
        (if (vector? vars)
            [vars clauses]
            [(vector vars)
            (mapcat (fn [[c a]]
                      [(if (not= c :else) (vector c) c) a])
              (partition 2 clauses))])]
     `(fn ~vars ~(clj-form vars clauses))))

(comment
  (do
    (def mul-pat1 `(~'* ~(variable 'x) ~(variable 'y)))
    (def linear-pat (->LinearPattern mul-pat1))
    (def core-pat (->CorePattern (match-fn x [* a b] {:a a :b b}
                           :else nil)))

    (def new-pat (->CorePattern (match-fn ([a b c] :seq) {:a a} :else nil)))

    (macroexpand '(match-fn ([a b c] :seq) {:a a} :else nil))

    (pat-match new-pat '(* 1 2))
    (println (pat-match new-pat 'y))
  ))