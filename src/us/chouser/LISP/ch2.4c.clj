(ns us.chouser.LISP.ch2.4c
  "A Lisp2 with an f-lookup that always returns in constant time. p41-42
   Try a multi-method dispatch for brevity."
  (:require [clojure.test :refer [is]]
            [clojure.core.match :refer [match]]
            [us.chouser.spread :refer [k.]]
            [us.chouser.LISP.test-lisp2 :as t])
  (:import (clojure.lang ISeq Symbol)))

(add-tap #'prn)

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(defn f-group
  "Return the dispatch value of expression e"
  [e env fenv]
  (-> [(type e)]
      (cond-> (seq? e)
        (conj (first e)))))

(ns-unmap *ns* 'feval)
(defmulti feval f-group)

(defn f-evlis [exps env fenv]
  (mapv #(feval % env fenv) exps))

(defn f-eprogn [exps env fenv]
  (transduce (map #(feval % env fenv))
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


(defmethod feval :default [e env fenv]
  (if-let [[f & args] (and (seq? e) e)]
    (evaluate-application f (f-evlis args env fenv) env fenv)
    (wrong "Cannot evaluate" e)))

(defmethod feval [Symbol] [e env fenv] (lookup e env))
(defmethod feval [Boolean] [e env fenv] e)
(defmethod feval [Number] [e env fenv] e)

(defmethod feval [ISeq 'quote] [[_ x] env fenv] x)
(defmethod feval [ISeq 'if] [[_ p t f] env fenv]
  (if (feval p env fenv)
    (feval t env fenv)
    (feval f env fenv)))
(defmethod feval [ISeq 'begin] [[_ & body] env fenv] (f-eprogn body env fenv))
(defmethod feval [ISeq 'set!] [[_ k v] env fenv]
  (update! k env (feval v env fenv)))

(defmethod feval [ISeq 'lambda] [[_ args & body] env fenv]
  (f-make-function args body env fenv))
(defmethod feval [ISeq 'let] [[_ bindings & body] env fenv]
  (f-eprogn body
            (reduce (fn [env [k e]]
                      (extend-env env
                                  (list k)
                                  (list (feval e env fenv))))
                    env
                    bindings)
            fenv))
(defmethod feval [ISeq 'flet] [[_ bindings & body] env fenv]
  (f-eprogn body env
            (extend-env fenv
                        (map first bindings)
                        (map (fn [[_ vars & body]]
                               (f-make-function vars body env fenv))
                             bindings))))
(defmethod feval [ISeq 'funcall] [[_ f & args] env fenv]
  (invoke (feval f env fenv)
          (f-evlis args env fenv)))

(defmethod feval [ISeq 'labels] [[_ defs & body] env fenv]
  (let [fenv (extend-env fenv
                         (map first defs)
                         (repeat (count defs) 'void))]
    (doseq [[k bindings & body] defs]
      (update! k fenv (f-make-function bindings body env fenv)))
    (f-eprogn body env fenv)))

(defmethod feval [ISeq 'function] [[_ arg] env fenv]
  (if (symbol? arg)
    (f-lookup arg fenv)
    (feval arg env fenv)))

(doto (k. lookup extend-env invoke
          :f-evaluate feval
          :env {} :fenv {})
  t/extend-env-tests
  t/main-tests
  t/late-missing-fn-test
  t/let-tests)

;;=== Global environment

(def env-global {})
(def fenv-global {})

(defn set-global [env-var var-name & value-seq]
  (alter-var-root
   env-var extend-env (list var-name) (if (seq value-seq)
                                        value-seq
                                        (list 'void))));; Why `void`?

(def definitial* (partial set-global #'env-global))

(definitial* 't true)
(definitial* 'f 'the-false-value)
(definitial* 'null ())  ;; nil isn't read as a symbol, so use null instead of adding an evaluate rule

(def definitial-function* "p38"
  (partial set-global #'fenv-global))

(defn defprimitive* [var-name var-value arity]
  (definitial-function* var-name
    (fn [values]
      (if (= arity (count values))
        (apply var-value values)
        (wrong "Incorrect arity"
               (list var-name values))))))

(defprimitive* 'car first 1)
(defprimitive* 'cdr rest 1)
(defprimitive* 'cons cons 2)
(defprimitive* '= = 2)
(defprimitive* '< < 2)
(defprimitive* '> > 2)
(defprimitive* '+ + 2)
(defprimitive* '- - 2)
(defprimitive* '* * 2)
(defprimitive* list list 2)

(defn eval* [expr]
  (feval expr env-global fenv-global))

(eval* '(labels ((fact (n) (if (= n 0)
                             1
                             (* n (fact (- n 1))))))
                (fact 6)))

(is (= 't
       (eval*
        '(funcall (labels ((even? (n) (if (= n 0) 't (odd? (- n 1))))
                           (odd? (n) (if (= n 0) 'f (even? (- n 1)))))
                          (function even?))
                  4))))
