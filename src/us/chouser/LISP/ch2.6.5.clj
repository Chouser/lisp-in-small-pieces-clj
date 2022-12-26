(ns us.chouser.LISP.ch2.6.5
  "The Lisp1 from ch1, but with a `let` that supports uninitialized variables so
  that `letrec` can be a macro, written with Clojure idioms rather than literal
  Scheme translation."
  (:require [clojure.test :refer [is]]
            [clojure.core.match :refer [match]]
            [us.chouser.spread :refer [k.]]))

(defn wrong [msg & args]
  (throw (ex-info (apply str "WRONG: " msg " "
                         (interpose " " (map pr-str args)))
                  (reduce merge {} (map meta args)))))

;; TODO: Update per p61
(defn lookup [id env]
  (if-let [a (get env id)]
    @a
    (wrong "No such binding" id)))

(defn update! [id env value]
  (if-let [a (get env id)]
    (reset! a value)
    (wrong "No such binding" id)))

(declare evaluate)

(def empty-begin (reify Object (toString [_] "empty-begin")))
(def the-false-value (reify Object (toString [_] "the-false-value")))

(defn eprogn [exps env]
  (transduce (map #(evaluate % env))
             (fn ([] empty-begin) ([a] a) ([a b] b))
             exps))

(defn extend-env [env variables values]
  (let [[normal-vars [_ rest-var]] (split-with #(not= '& %) variables)
        [normal-vals rest-val] (split-at (count normal-vars) values)]
    (cond
      (< (count normal-vals) (count normal-vars)) (wrong "Too less values")
      (and (seq rest-val) (not rest-var)) (wrong "Too much values")
      :else (-> env
                (merge (zipmap normal-vars (map atom normal-vals)))
                (cond-> rest-var (assoc rest-var (atom rest-val)))))))

(defn make-function [variables body env] ;; p19
  #(eprogn body (extend-env env variables %)))

(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

  ;; TODO: Update per p61
(defn do-let [bindings body env]
  (eprogn body
          (reduce (fn [env [k e]]
                    (extend-env env (list k) (list (evaluate e env))))
                  env
                  bindings)))

(defn evlis [exps env]
  (mapv #(evaluate % env) exps))

(defn evaluate [e env]
  (match e
         (e :guard symbol?) (lookup e env)
         (e :guard #(or (not (coll? %)) (vector? %))) e
         (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
         (['quote x] :seq) x
         (['if p t f] :seq) (if (not= (evaluate p env) the-false-value)
                              (evaluate t env)
                              (evaluate f env))
         (['begin & body] :seq) (eprogn body env)
         (['set! k v] :seq) (update! k env (evaluate v env))
         (['lambda args & body] :seq) (make-function args body env)
         (['let bindings & body] :seq) (do-let bindings body env)
         ([f & args] :seq) (invoke (evaluate f env) (evlis args env))))

;;=== Global environment

(def env-global {})

(defn set-global [env-var var-name & value-seq]
  (alter-var-root
   env-var extend-env (list var-name) (if (seq value-seq)
                                        value-seq
                                        (list 'void))));; Why `void`?

(def definitial* (partial set-global #'env-global))

(definitial* 't true)
(definitial* 'f the-false-value)
(definitial* 'null ())  ;; nil isn't read as a symbol, so use null instead of adding an evaluate rule

(defn defprimitive* [var-name var-value arity]
  (definitial* var-name
    (fn [values]
      (if (= arity (count values))
        (apply var-value values)
        (wrong "Incorrect arity"
               (list var-name values))))))

(defprimitive* 'car first 1)
(defprimitive* 'cdr rest 1)
(defprimitive* 'cons cons 2)
(defprimitive* '= #(or (= %1 %2) the-false-value) 2)
(defprimitive* '< #(or (< %1 %2) the-false-value) 2)
(defprimitive* '> #(or (> %1 %2) the-false-value) 2)
(defprimitive* '+ + 2)
(defprimitive* '- - 2)
(defprimitive* '* * 2)
(defprimitive* list list 2)

(defn eval* [expr]
  (evaluate expr env-global))

(is (= 1  (eval* '((lambda (a b) a) 1 2))))
(is (= 'a (eval* '(if true (quote a) 2))))
(is (= 5  (eval* '(if f 1 5))))
(is (= 3  (eval* '(begin 1 2 3))))
(is (= 9  (eval* '((lambda (a) (set! a 9) a) 5))))
(is (= empty-begin (eval* '(begin))))

(is (= 9
       (eval* '(let ((square (lambda (x) (* x x))))
                 (square 3)))))

(is (= 81
       (eval* '((let ((square (lambda (x) (* x x))))
                  (lambda (x) (square (square x))))
                3))))

(is (= '(7 12)
       (eval* '(let ((wat (lambda (a b)
                                  ((if (< a b) + *)
                                   a b))))
                 (cons (wat 3 4) (cons (wat 4 3) '()))))))

(is (= 9
       (eval* '(let ((add (lambda (a b) (+ a b))))
                 (add 4 5)))))

(is (= 7
       (eval* '(let ((bad (lambda () (missing-fn 1 2 3))))
                 7))))

(is (thrown-with-msg? Exception #"WRONG: No such binding missing-fn"
                      (eval* '(let ((bad (lambda () (missing-fn 1 2 3))))
                                (bad)))))

(is (= '2
       (eval* '(let ((a 2))
                 a))))

(is (= '3
       (eval* '(let ((a 1))
                 (let ((b 2))
                   (set! a 3))
                 a))))

(is (= 720
       (eval* '(letrec ((fact (lambda (n) (if (= n 0)
                                            1
                                            (* n (fact (- n 1)))))))
                       (fact 6)))))

:done
;; TODO: add tests for each use of wrong, and check for form/line/col debug info
