(ns factjor.core
  (:refer-clojure :exclude [drop]))

(defn- impl [word]
  (-> word meta ::impl))

(defn run
  ([queue] (run '() queue))
  ([stack queue]
    (reduce (fn [$ x]
              (if-let [f (-> x meta ::impl)]
                (apply f $)
                (conj $ x)))
             stack queue)))

(defn cat [& queue]
  (run queue))

(defn- curried [f]
  (with-meta (fn [& args]
               (curried (apply partial f args)))
             {::impl f}))

(defmacro defprim [name effect & body]
  (let [args (take-while (complement '#{--}) effect)]
    ` (def ~name (curried (fn [~@(reverse args) ~'& ~'$]
                            ~@body)))))

(defmacro defword [name effect & body]
  `(def ~name (curried (fn [~'& ~'$]
                         (run ~'$ ~(vec body))))))

(defprim clear [] nil)

(defprim execute [word]
  (apply (impl word) $))

(defprim dup [x -- x x]
  (conj $ x x))

(defprim swap [x y -- y x]
  (conj $ y x))

(defprim drop [x -- ]
  $)

(defprim call [word -- ]
  (apply (impl word) $))

(defprim over [x y -- x y x]
  (conj $ x y x $))

(defprim plus [x y -- z]
  (conj $ (+ x y)))

(defprim minus [x y -- z]
  (conj $ (- x y)))

(defword trip [x -- x x x] dup dup)


(comment

  (cat 1 2 clear)

  (cat 1 dup)

  (cat 5 10 swap)

  (cat 5 10 drop)

  (cat 3 5 plus)

  (cat 3 (plus 5))

  )
