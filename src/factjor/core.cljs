(ns factjor.core
  (:refer-clojure :exclude [drop keep
                            identical? =
                            + - * / < <= = >= > not=
                            inc dec
                            pr prn print println
                            not boolean
                            count
                            when when-not
                            while
                            first next rest nth])
  (:require [cljs.core :as cc]
            [factjor.runtime :as rt])
  (:use-macros [factjor.core :only (defprim defword call$)]
               [factjor.interop :only (cat-ops)]))


(defn run [& program]
  (-> program rt/create-interpreter rt/run :data))

;;;; Kernel

(defprim clear [] nil)

(defprim execute [word]
  (rt/execute $ word))

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

(cat-ops cc
  identical? [x y -- ?]
  =          [x y -- ?]
  not=       [x y -- ?])

;;; Linear order

(cat-ops cc
  < [x y -- ?]
  > [x y -- ?]
  <= [x y -- ?]
  >= [x y -- ?])


;;;; Basic data types

;;; Booleans

(cat-ops cc
  boolean [x -- ?]
  not [x -- ?])

(defprim or [x y]
  (conj $ (cljs.core/or x y)))

(defprim and [x y]
  (conj $ (cljs.core/and x y)))

;;; Numbers

(cat-ops cc
  + [x y -- z]
  - [x y -- z]
  * [x y -- z]
  / [x y -- z]
  inc [x -- y]
  dec [x -- y])

;; Dopey Clojure Reader bug! CLJ-873
(defprim div [x y -- z]
  (conj $ (clojure.core// x y)))


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

(cat-ops cc
  pr      [x --]
  prn     [x --]
  print   [x --]
  println [x --])

;;; Collections

(cat-ops cc
  count [coll -- n])

;;; Sequentials

(cat-ops cc
  first [coll -- x]
  nth [coll index -- x])
; nth-or ; 3 argument version of nth
;; Should return vectors & also provide -slice variants that return subvecs
;next clojure.core/next)
;rest clojure.core/rest)

