(ns factjor.core)

;;;; Forms for defining words

;; Anaphoric conventions:
;;    $   stack         $ is for Stack!
;;   <>   program       (inspired by Perl stdin)
;;   <$>  interpreter   This one should be obvious


(defn- word-name [sym]
  (symbol (str (ns-name *ns*)) (name sym)))

(defmacro defprim
  "Defines a word with an implementation body, which returns a new data stack.
  Anaphoric: stack $, interpreter <$>"
  [word effect & body]
  (let [args (take-while (complement '#{--}) effect)]
    `(def ~word
       (rt/primitive '~(word-name word)
         (fn ~word [interpreter#]
           (let [[~@(reverse args) ~'& ~'$] (:data interpreter#)
                 ~'<$> (assoc interpreter# :data ~'$)]
             ~@body))))))

(defmacro defword [word effect & body]
  `(def ~word
     (rt/word '~(word-name word) ~(vec body))))

(defmacro call$ [callable]
  `(:data (rt/call ~'<$> ~callable)))
