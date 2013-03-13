(ns factjor.interop
  "Utilities for interop with Applicative Clojure"
  (:require [factjor.runtime :as rt]))

;;TODO Move to appropriate namespace
(defn parse-stack-effect [effect]
  (let [[inputs [sep & outputs]] (split-with (complement '#{--}) effect)]
    (assert (= sep '--) "Expected '-- in stack effect")
    {:inputs (vec inputs) :outputs (vec outputs)}))

(defn- cat-op-form [f effect]
  (let [{:keys [inputs outputs]} (parse-stack-effect effect)
        input-set (set inputs)
        return-values (clojure.core/remove input-set outputs)
        result-sym (gensym)
        output-map {(first return-values) result-sym}
        conj-syms (map #(get output-map % %) outputs)]
    (assert (<= (count return-values) 1)
            (str "Unable to infer " f " output stack from: " effect))
  `(factjor.core/defprim ~(-> f name symbol) ~inputs
     (let [~result-sym (~f ~@inputs)]
       ~(if (empty? conj-syms)
          '$
          `(conj ~'$ ~@conj-syms))))))

(defmacro cat-ops
  "Defines words by wrapping functions in the namespace named ns. Each op-spec
  is an unqualified symbol and stack effect annotation. The number of inputs in
  the stack effect selects the overload, while names shared between the inputs
  and outputs are put back on the stack. If there is exactly one new name in
  the outputs, that's the function return value. See factjor.core for example."
  [ns & op-specs]
  (let [ns (name ns)]
    `(do ~@(for [[nm effect] (partition 2 op-specs)]
             (cat-op-form (symbol ns (name nm)) effect)))))
