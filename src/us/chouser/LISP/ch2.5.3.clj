(ns us.chouser.LISP.ch2.5.3
  (:require [clojure.core.reducers :as r]
            [clojure.core.match :refer [match]]
            [clojure.test :as t :refer [is]]))

;; This version drops the function environment (and thus function, flet, labels,
;; and funcall), but keeps a dynamic environment manipulated with bind/de and assoc/de

;;=== Environment fns

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(defn lookup [id env]
  (if-let [a (get env id)]
    @a
    (wrong "No such binding" id)))

(defn df-lookup [id fenv]
  (if-let [a (get fenv id)]
    @a
    (fn [values denv]
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

(defn r-last
  ([reducible] (reduce (fn [a b] b) nil reducible))
  ([reducible when-empty] (reduce (fn [a b] b) when-empty reducible)))

;;=== Eval

(def empty-begin 813)

(defn invoke [f args denv] ;; p51
  (if (fn? f)
    (f args denv)
    (wrong "Not a function" f)))

(declare dd-evaluate)

(defn dd-eprogn [e* env denv] ;; p51
  (if (empty? e*)
    empty-begin
    (r-last (r/map #(dd-evaluate % env denv) e*) ())))

(defn dd-make-function [variables body env] ;; p51
  (fn [values denv]
    (dd-eprogn body (extend-env env variables values) denv)))

;; Not given in the text, but used in the example!!
(defn dd-do-let [bindings body env denv]
  (dd-eprogn body
             (reduce (fn [env [k e]]
                       (extend-env env
                                   (list k)
                                   (list (dd-evaluate e env denv))))
                     env
                     bindings)
             denv))

;; Not given in the text. Note the similarity to `labels`
(defn dd-do-letrec [bindings body env denv]
  (let [env (extend-env env
                        (map first bindings)
                        (repeat (count bindings) 'void))]
    (doseq [[k e & body] bindings]
      (update! k env (dd-evaluate e env denv)))
    (dd-eprogn body env denv)))

#_
(defn dd-do-labels [defs body env denv]
  (let [env (extend-env env
                        (map first defs)
                        (repeat (count defs) 'void))]
    (doseq [[k bindings & body] defs]
      (update! k env (df-make-function bindings body env fenv)))
    (df-eprogn body env fenv denv)))

(defn dd-evlis [e* env denv] ;; p51
  (mapv #(dd-evaluate % env denv) e*))

(defn dd-evaluate [e env denv] ;; p50
  (match e
    (e :guard symbol?) (lookup e env)
    (e :guard #(or (not (coll? %)) (vector? %))) e
    (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
    (['quote x] :seq) x
    (['if p t f] :seq) (if (dd-evaluate p env denv)
                         (dd-evaluate t env denv)
                         (dd-evaluate f env denv))
    (['begin & body] :seq) (dd-eprogn body env denv)
    (['set! k v] :seq) (update! k env (dd-evaluate v env denv))
    (['lambda args & body] :seq) (dd-make-function args body env)
    (['let bindings & body] :seq) (dd-do-let bindings body env denv)
    (['letrec bindings & body] :seq) (dd-do-letrec bindings body env denv)
    ([f & args] :seq) (invoke (dd-evaluate f env denv)
                              (dd-evlis args env denv)
                              denv)))

;;=== Global environment

(def env-global {})

(defn set-global [env-var var-name & value-seq]
  (alter-var-root
   env-var extend-env (list var-name) (if (seq value-seq)
                                        value-seq
                                        (list 'void))));; Why `void`?

(def definitial* (partial set-global #'env-global))

(definitial* 't true)
(definitial* 'f 'the-false-value)
(definitial* 'null ())  ;; nil isn't read as a symbol, so use null instead of adding an evaluate rule

(defn defprimitive* [var-name var-value arity]
  (definitial* var-name
    (fn [values denv]
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

;;=== ch2.5.3 dynamic environment functions

(definitial* 'bind/de ;; p51
  (fn [[tag value thunk :as values] denv]
    (if (= 3 (count values))
      (invoke thunk () (extend-env denv (list tag) (list value)))
      (wrong "Incorrect arity" 'bind/de))))

(definitial* 'assoc/de ;; p51-52
  (fn [[tag default :as values] current-denv]
    (if (= 2 (count values))
      (if-let [*value (get current-denv tag)]
        @*value
        (invoke default (list tag) current-denv))
      (wrong "Incorrect arity" 'assoc/de))))

;;=== Tests

(defn eval* [expr]
  (dd-evaluate expr env-global {}))

(do ;; main-tests
  (is (= 1  (eval* '((lambda (a b) a) 1 2))))
  (is (= 'a (eval* '(if true (quote a) 2))))
  (is (= 5  (eval* '(if false 1 5))))
  (is (= 3  (eval* '(begin 1 2 3))))
  (is (= 9  (eval* '((lambda (a) (set! a 9) a) 5))))
  (is (= empty-begin (eval* '(begin)))))

;; Not given in the book text, but used in the example. Not quote sure what the
;; author had in mind, but this at least has the required signature.
(defprimitive* 'error #(do (prn :error %&) 100) 1)

;; example, p52
(is (= 8
       (eval* '(bind/de 'x 2
                        (lambda () (+ (assoc/de 'x error)
                                      (let ((x (+ (assoc/de 'x error)
                                                  (assoc/de 'x error))))
                                        (+ x (assoc/de 'x error)))))))))

(do ;; let-tests
  (is (= '2
         (eval* '(let ((a 2))
                   a))))
  (is (= '3
         (eval* '(let ((a 1))
                   (let ((b 2))
                     (set! a 3))
                   a)))))

(eval* '(letrec ((fact (lambda (n) (if (= n 0)
                                     1
                                     (* n (fact (- n 1)))))))
                (fact 6)))

(is (= 't
       (eval*
        '((letrec ((even? (lambda (n) (if (= n 0) 't (odd? (- n 1)))))
                   (odd? (lambda (n) (if (= n 0) 'f (even? (- n 1))))))
                  even?)
          4))))
