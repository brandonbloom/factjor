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

(defprotocol IWord)

(deftype Primitive [sym f]
  IWord
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
  )

(defn primitive [sym f]
  (Primitive. sym f))

(deftype Word [sym body]
  IWord
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
  )

(defn word [sym body]
  (Word. sym body))

(defn word? [obj]
  (satisfies? IWord obj))

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
  clojure.lang.Sequential
  (-call [callable interpreter]
    (:data (reduce execute interpreter callable))))

(extend-protocol FactjorObject
  )
