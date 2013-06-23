(ns factjor.runtime-test
  (:require [factjor.runtime :as rt]
            [factjor.core :as cat])
  (:use-macros [factjor.core :only (defword)]
               [factjor.test-helpers :only (deftest testing is)]))

(defword ten [-- x] 10)

(deftest runtime-tests
  (testing "Named"
    (is (= (map (juxt namespace name)
                [(rt/word 'x/y nil) (rt/primitive 'z/w nil)])
           [["x" "y"] ["z" "w"]]))
    )
  (testing "Decoding to literals"
    (is (= (rt/literal [5 "asdf" \A :b  'c])
                       [5 "asdf" \A :b ''c]))
    (is (= (rt/literal cat/clear) ; Primitive
           'factjor.core/clear))
    (is (= (rt/literal ten) ; Word
           'factjor.runtime-test/ten))
    (is (= (rt/literal (cat/+ 5 ten)) ; CurriedWord
           '(factjor.core/+ 5 factjor.runtime-test/ten)))
    )
  )
