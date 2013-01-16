(ns factjor.core-test
  (:use clojure.test)
  (:require [factjor.core :as cat :refer (go)]))

(deftest a-test
  (testing "values"
    (is (empty? (go)))
    (is (= (go 1 :a \x [cat/inc]) [[cat/inc] \x :a 1])))
  (testing "kernel operations"
    (is (empty? (go 1 2 3 cat/clear)))
    ;;TODO execute
    (is (= (go 5 [cat/inc] cat/call))))
  (testing "boolean ops"
    (is (= (go true true   cat/or) [true]))
    (is (= (go true false  cat/or) [true]))
    (is (= (go false true  cat/or) [true]))
    (is (= (go false false cat/or) [false]))
    (is (= (go nil 1 cat/or) [1]))
    (is (= (go true true   cat/and) [true]))
    (is (= (go true false  cat/and) [false]))
    (is (= (go false true  cat/and) [false]))
    (is (= (go false false cat/and) [false]))
    (is (= (go 1 2 cat/or) [1]))
    (is (= (go nil 2 cat/or) [2]))
    (is (= (go false cat/not [true])))
    (is (= (go 1 cat/not [false])))
    (is (= (go 5 cat/boolean [true])))
    (is (= (go nil cat/boolean [false]))))
  (testing "shuffle words"
    (is (= (go 1 2 3 4 5 cat/drop) [4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/drop2) [3 2 1]))
    (is (= (go 1 2 3 4 5 cat/drop3) [2 1]))
    (is (= (go 1 2 3 4 5 cat/nip) [5 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/nip2) [5 2 1]))
    (is (= (go 1 2 3 4 5 cat/dup) [5 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/dup2) [5 4 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/dup3) [5 4 3 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/over) [4 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/over2) [4 3 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/pick) [3 5 4 3 2 1]))
    (is (= (go 1 2 3 4 5 cat/swap) [4 5 3 2 1])))
  (testing "unary ops"
    (is (= (go 5 cat/inc) [6])))
  (testing "binary ops"
    (is (= (go 10 7 cat/-) [3])))
  (testing "dip combinators"
    (is (= (go 5 10 [2 cat/+] cat/dip) [10 7]))
    (is (= (go 5 10 15 [2 cat/+] cat/dip2) [15 10 7]))
    (is (= (go 5 10 15 20 [2 cat/+] cat/dip3) [20 15 10 7]))
    (is (= (go 5 10 15 20 25 [2 cat/+] cat/dip4) [25 20 15 10 7]))
    )
  (testing "keep combinators"
    (is (= (go 5 [cat/inc] cat/keep) [5 6]))
    (is (= (go 5 10 [cat/+] cat/keep2) [10 5 15]))
    ;;TODO keep3
    )
  (testing "cleave combinators"
    (is (= (go 5 [cat/inc] [cat/dec] cat/bi) [4 6]))
    ;;TODO bi2, bi3, tri, tri2, tri3
    )
  (testing "spread combinators"
    (is (= (go 5 10 [cat/inc] [cat/dec] cat/bi*) [9 6]))
    ;;TODO bi2*, tri*, tri2*
    )
  (testing "apply combinators"
    (is (= (go 5 10 [cat/inc] cat/bi&) [11 6]))
    ;;TODO bi2&, tri&, tri2&
    )
  (testing "conditional combinators"
    (is (= (go 5 true [cat/inc] [cat/dec] cat/branch) [6]))
    (is (= (go 5 false [cat/inc] [cat/dec] cat/branch) [4]))
    (is (= (go true 1 2 cat/choose) [1]))
    (is (= (go false 1 2 cat/choose) [2]))
    (is (= (go 1 false [cat/inc] cat/when)      [1]))
    (is (= (go 1 true  [cat/inc] cat/when)      [2]))
    (is (= (go 1 true  [cat/inc] cat/when-not)  [1]))
    (is (= (go 1 false  [cat/inc] cat/when-not) [2]))
    )
  (testing "looping combinators"
    (is (= (go 1 2 3 4 5 [cat/dup 3 cat/>=] [cat/drop] cat/while) [2 1])))
  ;(testing "Clojure interop: currying"
  ;  (is (= (go 5 (cat/- 3)) [2]))
  ;  (is (= (go (cat/- 5 3)) [2])))
  )
