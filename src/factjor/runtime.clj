(ns factjor.runtime)

(defrecord Interpreter [data call program])

(defn create-interpreter [program]
  (Interpreter. nil nil program))

(defprotocol ICallable
  (-call [callable interpreter]))

(defn callable? [obj]
  (satisfies? ICallable obj))

(defprotocol FactjorObject
  (-literal [obj]))

(defprotocol IWord
  (-sym [obj]))

(defn word? [obj]
  (satisfies? IWord obj))

(defn sym [x]
  (when (word? x)
    (-sym x)))

(deftype CurriedWord [word args] ;TODO  _hash _meta
  IWord
  java.lang.Object
  (toString [_]
    (str word " " args))
  ICallable
  (-call [_ interpreter]
    (-call word (apply update-in interpreter [:data] conj args)))
  FactjorObject
  (-literal [_]
    (list* (-sym word) args))
  )

(defn curried-word [word & args]
  (CurriedWord. word args))

(deftype Primitive [sym f] ;TODO  _hash _meta
  IWord
  (-sym [_] sym)
  clojure.lang.Named
  (getNamespace [this]
    (namespace sym))
  (getName [_]
    (name sym))
  java.lang.Object
  (toString [_]
    (str sym " " f))
  ICallable
  (-call [_ interpreter]
    (f interpreter))
  FactjorObject
  (-literal [_]
    sym)
  clojure.lang.IFn
  (invoke [this]
    (curried-word this))
  (invoke [this a]
    (curried-word this a))
  (invoke [this a b]
    (curried-word this a b))
  (invoke [this a b c]
    (curried-word this a b c))
  (invoke [this a b c d]
    (curried-word this a b c d))
  (invoke [this a b c d e]
    (curried-word this a b c d e))
  (invoke [this a b c d e f]
    (curried-word this a b c d e f))
  (invoke [this a b c d e f g]
    (curried-word this a b c d e f g))
  (invoke [this a b c d e f g h]
    (curried-word this a b c d e f g h))
  (invoke [this a b c d e f g h i]
    (curried-word this a b c d e f g h i))
  (invoke [this a b c d e f g h i j]
    (curried-word this a b c d e f g h i j))
  (invoke [this a b c d e f g h i j k]
    (curried-word this a b c d e f g h i j k))
  (invoke [this a b c d e f g h i j k l]
    (curried-word this a b c d e f g h i j k l))
  (invoke [this a b c d e f g h i j k l m]
    (curried-word this a b c d e f g h i j k l m))
  (invoke [this a b c d e f g h i j k l m n]
    (curried-word this a b c d e f g h i j k l m n))
  (invoke [this a b c d e f g h i j k l m n o]
    (curried-word this a b c d e f g h i j k l m n o))
  (invoke [this a b c d e f g h i j k l m n o p]
    (curried-word this a b c d e f g h i j k l m n o p))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (curried-word this a b c d e f g h i j k l m n o p q))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (curried-word this a b c d e f g h i j k l m n o p q r))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (curried-word this a b c d e f g h i j k l m n o p q r s))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (curried-word this a b c d e f g h i j k l m n o p q r s t))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (apply curried-word this a b c d e f g h i j k l m n o p q r s t rest))
  )

(defn primitive [sym f]
  (Primitive. sym f))

(deftype Word [sym body]
  IWord
  (-sym [_] sym)
  clojure.lang.Named
  (getNamespace [this]
    (namespace sym))
  (getName [_]
    (name sym))
  java.lang.Object
  (toString [_]
    (str sym " " body))
  ICallable
  (-call [_ interpreter]
    (-call body interpreter))
  FactjorObject
  (-literal [_]
    sym)
  clojure.lang.IFn
  (invoke [this]
    (curried-word this))
  (invoke [this a]
    (curried-word this a))
  (invoke [this a b]
    (curried-word this a b))
  (invoke [this a b c]
    (curried-word this a b c))
  (invoke [this a b c d]
    (curried-word this a b c d))
  (invoke [this a b c d e]
    (curried-word this a b c d e))
  (invoke [this a b c d e f]
    (curried-word this a b c d e f))
  (invoke [this a b c d e f g]
    (curried-word this a b c d e f g))
  (invoke [this a b c d e f g h]
    (curried-word this a b c d e f g h))
  (invoke [this a b c d e f g h i]
    (curried-word this a b c d e f g h i))
  (invoke [this a b c d e f g h i j]
    (curried-word this a b c d e f g h i j))
  (invoke [this a b c d e f g h i j k]
    (curried-word this a b c d e f g h i j k))
  (invoke [this a b c d e f g h i j k l]
    (curried-word this a b c d e f g h i j k l))
  (invoke [this a b c d e f g h i j k l m]
    (curried-word this a b c d e f g h i j k l m))
  (invoke [this a b c d e f g h i j k l m n]
    (curried-word this a b c d e f g h i j k l m n))
  (invoke [this a b c d e f g h i j k l m n o]
    (curried-word this a b c d e f g h i j k l m n o))
  (invoke [this a b c d e f g h i j k l m n o p]
    (curried-word this a b c d e f g h i j k l m n o p))
  (invoke [this a b c d e f g h i j k l m n o p q]
    (curried-word this a b c d e f g h i j k l m n o p q))
  (invoke [this a b c d e f g h i j k l m n o p q r]
    (curried-word this a b c d e f g h i j k l m n o p q r))
  (invoke [this a b c d e f g h i j k l m n o p q r s]
    (curried-word this a b c d e f g h i j k l m n o p q r s))
  (invoke [this a b c d e f g h i j k l m n o p q r s t]
    (curried-word this a b c d e f g h i j k l m n o p q r s t))
  (invoke [this a b c d e f g h i j k l m n o p q r s t rest]
    (apply curried-word this a b c d e f g h i j k l m n o p q r s t rest))
  )

(defn word [sym body]
  (Word. sym body))

(defn call [interpreter callable]
  (let [interpreter* (update-in interpreter [:call] conj callable)]
    (try
      (let [stack (-call callable interpreter*)]
        (assoc interpreter :data stack))
    (catch clojure.lang.ExceptionInfo e
      (throw e))
    (catch Exception e
      (ex-info (str "Factjor exception: " e)
               {:interpreter interpreter*}
               e)))))

(defn execute [interpreter word]
  (if (word? word)
    (call interpreter word)
    (update-in interpreter [:data] conj word)))

(defn step [interpreter]
  (let [word (first (:program interpreter))
        interpreter* (update-in interpreter [:program] next)]
    (execute interpreter* word)))

(defn run [interpreter]
  (loop [interpreter* interpreter]
    (if (:program interpreter*)
      (recur (step interpreter*))
      interpreter*)))

(extend-protocol ICallable
  clojure.lang.Sequential ; should be on Vector and Quotation, not Sequential ;TODO why?
  (-call [callable interpreter]
    (:data (reduce execute interpreter callable))))

(extend-protocol FactjorObject
  )
