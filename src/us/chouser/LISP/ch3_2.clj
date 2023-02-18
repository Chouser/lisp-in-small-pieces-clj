(ns us.chouser.LISP.ch3-2
  ""
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

(defn eprogn [exps env k]
  (transduce (map #(evaluate % env k))
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

(defn make-function [variables body env k]
  #(eprogn body (extend-env env variables %) k))

#_
(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defmulti invoke (fn [f v* env k] (type f)))
(defmulti resume (fn [k v] (:type k)))

(defmethod invoke :default [f v* env k]
  (wrong "not a function" f env k))

(defmethod invoke clojure.lang.Fn [f v* env k]
  (resume k (f v*)))

(defmethod resume :default [k v]
  (wrong "Unknown continuation" k))

(defn make-if-cont [k et ef env]
  {:type 'if-cont
   :k k
   :et et
   :ef ef
   :env env})

(defn evaluate-if [ec et ef env k]
  (evaluate ec env (make-if-cont k et ef env)))

(defmethod resume 'if-cont [k v]
  (evaluate (if (not= v the-false-value) (:et k) (:ef k))
            (:env k)
            (:k k)))

#_
(defmethod invoke 'primitive [ff vv* rr kk]
  ((primitive-address ff) vv* rr kk))

;; p61: `let` with simultaneous bindings, supporting uninitialized variables
(defn do-let [bindings body env k]
  (eprogn body
          (extend-env env
                      (map #(if (symbol? %) % (first %)) bindings)
                      (map #(if (symbol? %)
                              the-uninitialized-marker
                              (evaluate (second %) env k))
                           bindings))
          k))

(defn do-letrec [bindings body env k]
  (let [temps (map #(symbol (str "_temp_" %)) (range (count bindings)))]
    (evaluate
     `(~'let ~(map first bindings)
       (~'let ~(map list temps (map second bindings))
        ~@(map #(list 'set! %1 %2) (map first bindings) temps)
        ~@body))
     env
     k)))

(defn evlis [exps env k]
  (mapv #(evaluate % env k) exps))

(defn evaluate [e env k]
  (match e
         (e :guard symbol?) (lookup e env)
         (e :guard #(or (not (coll? %)) (vector? %))) (resume k e)
         (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
         (['quote x] :seq) (resume k x)
         (['if p t f] :seq) (evaluate-if p t f env k)
         (['begin & body] :seq) (eprogn body env k)
         (['set! variable value] :seq) (update! variable env
                                                (evaluate value env k))
         (['lambda args & body] :seq) (make-function args body env k)
         (['let bindings & body] :seq) (do-let bindings body env k)
         (['letrec bindings & body] :seq) (do-letrec bindings body env k)
         ([f & args] :seq) (invoke (evaluate f env k)
                                   (evlis args env k)
                                   env
                                   k)))

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

(defmethod resume 'top-eval [k v]
  v
  #_
  (deliver (:promise k) v))

(defn eval* [expr]
  (evaluate expr env-global {:type 'top-eval})
  #_
  (let [p (promise)]
    (evaluate expr env-global {:type 'top-eval
                               :promise p})
    @p))

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

#_
(is (thrown-with-msg? Exception #"Uninitialized"
                      (eval* '(letrec ((x (+ x 1))) x))))

#_
(is (thrown-with-msg? Exception #"No such"
                      (eval* '(letrec ((x (+ y 1))) y))))

;; TODO: add tests for each use of wrong, and check for form/line/col debug info
