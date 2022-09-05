(ns us.chouser.LISP.ch2.4b
  "A Lisp2 with an f-lookup that always returns in constant time. p41-42
   This time with a more idiomatic Clojure implementation."
  (:require [clojure.test :refer [is]]
            [clojure.core.match :refer [match]]
            [us.chouser.spread :refer [k.]]
            [us.chouser.LISP.test-lisp2 :as t]))

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(declare f-evaluate)

(defn f-evlis [exps env fenv]
  (mapv #(f-evaluate % env fenv) exps))

(defn f-eprogn [exps env fenv]
  (transduce (map #(f-evaluate % env fenv))
             (fn ([] ()) ([a] a) ([a b] b))
             exps))

(defn lookup [id env]
  (if-let [a (get env id)]
    @a
    (wrong "No such binding" id)))

(defn f-lookup [id fenv]
  (if-let [a (get fenv id)]
    @a
    (fn [values]
      (wrong "No such functional binding" id))))

(defn update! [id env value]
  (if-let [a (get env id)]
    (reset! a value)
    (wrong "No such binding" id)))

(defn extend-env [env variables values]
  (let [[normal-vars [_ rest-var]] (split-with #(not= '& %) variables)
        [normal-vals rest-val] (split-at (count normal-vars) values)]
    (cond
      (< (count normal-vals) (count normal-vars)) (wrong "Too less values")
      (and (seq rest-val) (not rest-var)) (wrong "Too much values")
      :else (-> env
                (merge (zipmap normal-vars (map atom normal-vals)))
                (cond-> rest-var (assoc rest-var (atom rest-val)))))))

(defn f-make-function [variables body env fenv]
  (fn [values]
    (f-eprogn body (extend-env env variables values) fenv)))

(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defn evaluate-application [f args env fenv]
  (match f
    (f :guard symbol?) ((f-lookup f fenv) args)
    (['lambda params & body] :seq) (f-eprogn body
                                             (extend-env env params args)
                                             fenv)
    :else (wrong "Incorrect functional term" f)))

(defn f-do-let [bindings body env fenv]
  (f-eprogn body
            (reduce (fn [env [k e]]
                      (extend-env env
                                  (list k)
                                  (list (f-evaluate e env fenv))))
                    env
                    bindings)
            fenv))

(defn f-do-flet [bindings body env fenv]
  (f-eprogn body env
            (extend-env fenv
                        (map first bindings)
                        (map (fn [[_ vars & body]]
                               (f-make-function vars body env fenv))
                             bindings))))

(defn f-do-labels [defs env fenv]
  (let [fenv (extend-env fenv
                         (map first defs)
                         (repeat (count defs) 'void))]
    (doseq [[k bindings & body] defs]
      (update! k fenv (f-make-function bindings body env fenv)))))

(defn f-evaluate [e env fenv]
  (match e
    (e :guard symbol?) (lookup e env)
    (e :guard #(or (not (coll? %)) (vector? %))) e
    (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
    (['quote x] :seq) x
    (['if p t f] :seq) (if (f-evaluate p env fenv)
                         (f-evaluate t env fenv)
                         (f-evaluate f env fenv))
    (['begin & body] :seq) (f-eprogn body env fenv)
    (['set! k v] :seq) (update! k env (f-evaluate v env fenv))
    (['lambda args & body] :seq) (f-make-function args body env fenv)
    (['let bindings & body] :seq) (f-do-let bindings body env fenv)
    (['function (sym :guard symbol?)] :seq) (f-lookup sym fenv)
    (['function (['lambda a & b] :seq)] :seq) (f-make-function a b env fenv)
    (['function & _] :seq) (wrong "Incorrect function" (second e))
    (['flet bindings & body] :seq) (f-do-flet bindings body env fenv)
    (['labels defs] :seq) (f-do-labels defs env fenv)
    ([f & args] :seq) (evaluate-application f (f-evlis args env fenv) env fenv)))

(doto (k. lookup extend-env invoke f-evaluate
          :env {} :fenv {})
  t/extend-env-tests
  t/main-tests
  t/late-missing-fn-test
  t/let-tests)