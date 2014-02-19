(ns ^{:doc "Do some Pattern matching"
      :author "Zenna Tavares"}
  veneer.pattern.match)

(defn square-pattern
  "A pattern is a tuple of two functions"
  [term]
  (if (and (>= (count term) 2)
           (= (first term) 'square))
      {'x (rest term)}))

(defn square-rewrite
  "The rewrite needs to know, in this case that it is
   the second term that needs to be rewritten.
   Or more generally what the bound variables are.
   So it could return the original term but with"
  [{x 'x} term]
  '(* x x))

(defn apply-pattern
  [term [lhs rhs]]
  (-> term lhs rhs))

(comment
  (def square-rewrite [square-pattern square-rewrite])
  (apply-pattern '[square x] square-rewrite))