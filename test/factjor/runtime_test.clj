(ns factjor.runtime-test
  (:use clojure.test)
  (:require [factjor.runtime :as rt]
            [factjor.core :as cat :refer (defword)]))

(defword ten [-- x] 10)

(deftest runtime-tests
  (testing "Named"
    (is (= (map (juxt namespace name)
                [(rt/word 'x/y nil) (rt/primitive 'z/w nil)])
           [["x" "y"] ["z" "w"]]))
    )
  (testing "Decoding to literals"
    (is (= (rt/literal [5 1/2 "asdf" \A :b  'c])
                       [5 1/2 "asdf" \A :b ''c]))
    (is (= (rt/literal cat/clear) ; Primitive
           'factjor.core/clear))
    (is (= (rt/literal ten) ; Word
           'factjor.runtime-test/ten))
    (is (= (rt/literal (cat/+ 5 ten)) ; CurriedWord
           '(factjor.core/+ 5 factjor.runtime-test/ten)))
    )
  )
