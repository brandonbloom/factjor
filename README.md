# factjor

A Clojure library inspired by [Factor](http://www.factorcode.org)

The name "Factjor" is a mashup of "Factor" and "Clojure".
It is pronounced "Fact-sure".


## Motivation

Stack languages have several very nice properties:

1. Concatenation is function composition.
2. Side effects occur from left to right.
3. Complex pipelines can be constructed without naming values ("Point Free").

These properties enable traditionally side-effectual code to be converted into
a pure function which returns a (potentially lazy) sequence of words comprising
a concatenative program. Additionally, the linear nature of execution makes
concatenative languages a natural fit for streaming protocols.

See also: [Why Concatenative Programming Matters](http://evincarofautumn.blogspot.com/2012/02/why-concatenative-programming-matters.html).


## Usage

```clojure
(require '[factjor.core :as cat :refer (defword)])

;; Math is a terrible fit for concatenative languages, but it's easy to try.
;; The run function executes a queue of words and returns the stack after
;; execution. The stack is a normal Clojure list, whose head is the stack top.
(cat/run 10 7 cat/-) ;=> (3)

;; You can define new words in terms of other words.
;; What looks like an argument list is actually just documentation.
;; It describes the "stack effect" of the word.
;; Sadly, 'cat// must be 'cat/div because of a Clojure reader bug.
(defword avg2 [x y -- avg] cat/+ 2 cat/div)
(cat/run 10 20 avg2) ;=> (15)

;; Sequences of words, called "quotations", enable higher-order words,
;; called "combinators". Use cat/call to invoke a quotation.
(cat/run 5 [2 cat/+] cat/call) ;=> (7)
(cat/run 5 10 [2 cat/+] cat/dip) ;=> (10 7)

;; Word objects can be applied early to curry values to the right:
(cat/run 5 (cat/- 2)) ;=> 3
```

Lots more docs to follow when I release the project that motivated Factjor.


## License

Copyright © 2013 Brandon Bloom

Distributed under the Eclipse Public License, the same as Clojure.
