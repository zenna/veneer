(ns veneer.match-test
  (:require [clojure.test :refer :all]
            [veneer.pattern.match :refer :all]
            [veneer.pattern.match-macros :refer [match-fn]]))

(deftest good-test
  (testing "Pattern Matcher"
    (let [mul-pat1 `(~'* ~(variable 'x) ~(variable 'y))
          new-pat (->CorePattern (match-fn ([a b c] :seq) {:a a} :else nil))]
      (is  (= {:a '*} (pat-match new-pat '(* 1 2)))))))
