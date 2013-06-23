(ns factjor.test-helpers)

(defmacro deftest [name & body]
  `(do ~@body))

(defmacro testing [description & body]
  `(do ~@body))

(defmacro is [expr]
  `(assert ~expr))
