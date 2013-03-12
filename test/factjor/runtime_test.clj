(ns factjor.runtime-test
  (:use clojure.test)
  (:require [factjor.runtime :as rt]))

(deftest a-test
  (testing "Named"
    (is (= (map (juxt namespace name)
                [(rt/word 'x/y nil) (rt/primitive 'z/w nil)])
           [["x" "y"] ["z" "w"]]))
    ))
