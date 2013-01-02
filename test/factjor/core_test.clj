(ns factjor.core-test
  (:use clojure.test)
  (:require [factjor.core :as cat :refer (run)]))

(deftest a-test
  (testing "values"
    (is (= (run) []))
    (is (= (run 1 :a \x [cat/inc]) [[cat/inc] \x :a 1])))
  (testing "kernel operations"
    (is (empty? (run 1 2 3 cat/clear)))
    ;;TODO execute
    (is (= (run 5 [cat/inc] cat/call))))
  (testing "shuffle words"
    (is (= (run 1 2 3 4 5 cat/drop) [4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/drop2) [3 2 1]))
    (is (= (run 1 2 3 4 5 cat/drop3) [2 1]))
    (is (= (run 1 2 3 4 5 cat/nip) [5 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/nip2) [5 2 1]))
    (is (= (run 1 2 3 4 5 cat/dup) [5 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/dup2) [5 4 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/dup3) [5 4 3 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/over) [4 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/over2) [4 3 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/pick) [3 5 4 3 2 1]))
    (is (= (run 1 2 3 4 5 cat/swap) [4 5 3 2 1])))
  (testing "unary ops"
    (is (= (run 5 cat/inc) [6])))
  (testing "binary ops"
    (is (= (run 10 7 cat/-) [3])))
  (testing "dip combinators"
    (is (= (run 5 10 [2 cat/+] cat/dip) [10 7]))
    (is (= (run 5 10 15 [2 cat/+] cat/dip2) [15 10 7]))
    (is (= (run 5 10 15 20 [2 cat/+] cat/dip3) [20 15 10 7]))
    (is (= (run 5 10 15 20 25 [2 cat/+] cat/dip4) [25 20 15 10 7]))
    )
  (testing "keep combinators"
    (is (= (run 5 [cat/inc] cat/keep) [5 6]))
    (is (= (run 5 10 [cat/+] cat/keep2) [10 5 15]))
    ;;TODO keep3
    )
  (testing "cleave combinators"
    (is (= (run 5 [cat/inc] [cat/dec] cat/bi) [4 6]))
    ;;TODO bi2, bi3, tri, tri2, tri3
    )
  (testing "spread combinators"
    (is (= (run 5 10 [cat/inc] [cat/dec] cat/bi*) [9 6]))
    ;;TODO bi2*, tri*, tri2*
    )
  (testing "apply combinators"
    (is (= (run 5 10 [cat/inc] cat/bi&) [11 6]))
    ;;TODO bi2&, tri&, tri2&
    )
  )
