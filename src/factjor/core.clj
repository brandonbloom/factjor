(ns factjor.core
  (:refer-clojure :exclude [drop keep
                            identical? =
                            + - * / < <= = >= > not=
                            inc dec
                            pr prn print println
                            and or not boolean
                            when when-not
                            while])
  (:require [factjor.runtime :as rt]))


(defn go [& program]
  (-> program rt/create-interpreter rt/run :data))


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
       (factjor.runtime.Primitive. '~(word-name word)
         (fn ~word [interpreter#]
           (let [[~@(reverse args) ~'& ~'$] (:data interpreter#)
                 ~'<$> (assoc interpreter# :data ~'$)]
             ~@body))))))

(defmacro defword [word effect & body]
  `(def ~word
     (factjor.runtime.Word. '~(word-name word) ~(vec body))))


;;; Convenience macros for making words out of Clojure functions

(defmacro defvoid1 [name f]
  `(defprim ~name [x#] (~f x#) ~'$))

(defmacro defop1 [name f]
  `(defprim ~name [x#] (conj ~'$ (~f x#))))

(defmacro defop2 [name f]
  `(defprim ~name [x# y#] (conj ~'$ (~f x# y#))))


;;;; Kernel

(defprim clear [] nil)

(defprim execute [word]
  (rt/execute $ word))

(defmacro call$ [callable]
  `(:data (rt/call ~'<$> ~callable)))

(defprim call [quot --]
  (call$ quot))

(defprim callable? [obj -- bool]
  (conj $ (rt/callable? obj)))

(defprim word? [obj -- bool]
  (conj $ (rt/word? obj)))


;;; Shuffle words

;; Removing stack elements
(defprim drop [x] $)
(defprim drop2 [x y] $)
(defprim drop3 [x y z] $)
(defprim nip [x y] (conj $ y))
(defprim nip2 [x y z] (conj $ z))

;; Duplicating stack elements
(defprim dup [x] (conj $ x x))
(defprim dup2 [x y] (conj $ x y x y))
(defprim dup3 [x y z] (conj $ x y z x y z))
(defprim over [x y] (conj $ x y x))
(defprim over2 [x y z] (conj $ x y z x y))
(defprim pick [x y z] (conj $ x y z x))

;; Permuting stack elements
(defprim swap [x y] (conj $ y x))


;;;; Objects

;;; Equality

(defop2 identical? clojure.core/identical?)
(defop2 = clojure.core/=)
(defop2 not= clojure.core/not=)

;;; Linear order

(defop2 < clojure.core/<)
(defop2 > clojure.core/>)
(defop2 <= clojure.core/<=)
(defop2 >= clojure.core/>=)


;;;; Basic data types

;;; Booleans

(defop1 boolean clojure.core/boolean)
(defop2 or clojure.core/or)
(defop2 and clojure.core/and)
(defop1 not clojure.core/not)

;;; Numbers

(defop2 + clojure.core/+)
(defop2 - clojure.core/-)
(defop2 * clojure.core/*)
(defop2 / clojure.core//)
(defop2 div clojure.core//) ;;TODO dopey divide operator!

(defop1 inc clojure.core/inc)
(defop1 dec clojure.core/dec)


;;;; Combinators

;;; Dataflow combinators
; Factor uses number prefixes, such as 2dip, but Clojure can't, so we suffix

;; Preserving combinators
; dip combinators: invoke the quotation at the top of stack, hiding some values
(defprim dip  [x       quot] (conj (call$ quot) x      ))
(defprim dip2 [x y     quot] (conj (call$ quot) x y    ))
(defprim dip3 [x y z   quot] (conj (call$ quot) x y z  ))
(defprim dip4 [x y z w quot] (conj (call$ quot) x y z w))
; keep combinators: invoke a quotation and restore some number of values
(defword keep [..a x quot[..a x -- ..b] -- ..b x]
  over [call] dip)
(defword keep2 [..a x y quot[..a x y -- ..b] -- ..b x y]
  [dup2] dip dip2)
(defword keep3 [..a x y z quot[..a x y z -- ..b] -- ..b x y z]
  [dup2] dip dip2)

;; Cleave combinators: apply multiple quotations to a single value
(defword bi [x p q --] [keep] dip call)
(defword bi2 [x y p q --] [keep2] dip call)
(defword bi3 [x y z p q --] [keep3] dip call)
(defword tri [x p q r --] [[keep] dip keep] dip call)
(defword tri2 [x y p q r --] [[keep2] dip keep2] dip call)
(defword tri3 [x y z p q r --] [[keep3] dip keep3] dip call)

;; Spread combinators: apply multiple quotations to multiple values
(defword bi* [x y p q --] [dip] dip call)
(defword bi2* [x y p q --] [dip2] dip call)
(defword tri* [x y z p q r --] [[dip2] dip dip] dip call)
(defword tri2* [u v w x y z p q r --] [[dip4] dip2 bi2*])
;WORD spread [objs... seq --] ;TODO

;; Apply combinators: apply a single quotation to multiple values
;; Factor uses @ suffix, but we can't do that in Clojure, so use &
(defword bi& [x y quot --] dup bi*)
(defword bi2& [w x y z quot --] dup bi2*)
(defword tri& [x y z quot --] dup dup tri*)
(defword tri2& [u v w x y z quot --] dup dup tri2*)


;;; Conditional Combinators

(defprim branch [bool then else --]
  (call$ (if bool then else)))

(defprim choose [bool if-true if-false -- value]
  (conj $ (if bool if-true if-false)))

(defprim when [bool then]
  (if bool (call$ then) $))

(defprim when-not [bool else]
  (if bool $ (call$ else)))


;;; Looping combinators

(defprim while [pred body --]
  (loop [<$> <$>]
    (let [<$> (rt/call <$> pred)
          [bool & $] (:data <$>)
          <$> (assoc <$> :data $)]
      (if bool
        (recur (rt/call <$> body))
        $))))


;;; Compositional combinators


;;; Short-circuit combinators



;;;; Other stdlib stuff

;;; Printing

(defvoid1 pr clojure.core/pr)
(defvoid1 prn clojure.core/prn)
(defvoid1 print clojure.core/print)
(defvoid1 println clojure.core/println)
