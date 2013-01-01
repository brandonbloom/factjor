(ns factjor.core
  (:refer-clojure :exclude [drop]))

(defn- impl [word]
  (-> word meta ::impl))

(defn- execute* [stack word]
  (if-let [f (-> word meta ::impl)]
    (apply f stack)
    (conj stack word)))

(defn run
  ([queue] (run '() queue))
  ([stack queue]
    (reduce execute* stack queue)))

(defn cat [& queue]
  (run queue))

(defn- curried [f]
  (with-meta (fn [& args]
               (curried (apply partial f args)))
             {::impl f}))

(defmacro defprim [name effect & body]
  (let [args (take-while (complement '#{--}) effect)]
    ` (def ~name (curried (fn ~name [~@(reverse args) ~'& ~'$]
                            ~@body)))))

(defmacro defword [name effect & body]
  `(def ~name (curried (fn ~name [~'& ~'$]
                         (run ~'$ ~(vec body))))))


;;; Kernel

(defprim clear [] nil)

(defprim execute [word]
  (execute* $ word))

(defprim call [quotation -- ]
  (run $ quotation))


;;; Shuffle words

(defprim dup [x -- x x]
  (conj $ x x))

(defprim swap [x y -- y x]
  (conj $ y x))

(defprim drop [x -- ]
  $)

(defprim over [x y -- x y x]
  (conj $ x y x $))

(defword trip [x -- x x x] dup dup)


;;; Other stuff

(defprim plus [x y -- z]
  (conj $ (+ x y)))

(defprim minus [x y -- z]
  (conj $ (- x y)))


(comment

  (cat 1 2 clear)

  (cat 1 dup)

  (cat 5 10 swap)

  (cat 5 10 drop)

  (cat 3 5 plus)

  (cat 3 (plus 5))

  (cat 5 [10 plus] call)

  )
