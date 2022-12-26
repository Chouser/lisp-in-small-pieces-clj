(ns us.chouser.LISP.ch2.6.5
  "The Lisp1 from ch1, but with a `let` that supports uninitialized variables so
  that `letrec` can be a macro, written with Clojure idioms rather than literal
  Scheme translation."
  (:require [clojure.test :refer [is]]
            [clojure.core.match :refer [match]]))

(defn wrong [msg & args]
  (throw (ex-info (apply str "WRONG: " msg " "
                         (interpose " " (map pr-str args)))
                  (reduce merge {} (map meta args)))))

(def the-uninitialized-marker (reify Object (toString [_] "uninitialized")))

(defn lookup [id env]
  (if-let [a (get env id)]
    (let [v @a]
      (if (= the-uninitialized-marker v)
        (wrong "Uninitialized binding" id)
        v)
      @a)
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

;; p61: `let` with simultaneous bindings, supporting uninitialized variables
(defn do-let [bindings body env]
  (eprogn body
          (extend-env env
                      (map #(if (symbol? %) % (first %)) bindings)
                      (map #(if (symbol? %)
                              the-uninitialized-marker
                              (evaluate (second %) env))
                           bindings))))

(defn do-letrec [bindings body env]
  (let [temps (map #(symbol (str "_temp_" %)) (range (count bindings)))]
    (evaluate
     `(~'let ~(map first bindings)
       (~'let ~(map list temps (map second bindings))
        ~@(map #(list 'set! %1 %2) (map first bindings) temps)
        ~@body))
     env)))

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
         (['letrec bindings & body] :seq) (do-letrec bindings body env)
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

(is (= 't
       (eval*
        '((letrec ((even? (lambda (n) (if (= n 0) 't (odd? (- n 1)))))
                   (odd? (lambda (n) (if (= n 0) 'f (even? (- n 1))))))
                  even?)
          4))))

(is (thrown-with-msg? Exception #"Uninitialized"
                      (eval* '(letrec ((x (+ x 1))) x))))

(is (thrown-with-msg? Exception #"No such"
                      (eval* '(letrec ((x (+ y 1))) y))))

;; TODO: add tests for each use of wrong, and check for form/line/col debug info
