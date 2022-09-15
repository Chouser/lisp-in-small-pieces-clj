(ns us.chouser.LISP.ch2.5.2
  (:require [clojure.core.reducers :as r]
            [clojure.core.match :refer [match]]
            [us.chouser.spread :refer [k.]]
            [clojure.test :as t :refer [is]]))

(def env-global {})
(def fenv-global {})

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

;; These from 45 and 46:

(declare df-evaluate)

(defn df-evlis
  "Not given in the book text."
  [e* env fenv denv]
  (mapv #(df-evaluate % env fenv denv) e*))

(defn df-eprogn [e* env fenv denv]
  (r-last (r/map #(df-evaluate % env fenv denv) e*) ()))

(defn cl-update! [id env denv value]
  "Not given in the book text"
  (if-let [a (get env id)]
    (if-not (= ::dynamic a)
      (reset! a value)
      ;; lookup in the current dynamic environment
      (if-let [a (get denv id)]
        (reset! a value)
        ;; default to the global lexical environment
        (update! id env-global value)))
    (wrong "No such binding" id)))

;; p49
(defn special-extend [env variables]
  (into env (zipmap variables (repeat ::dynamic))))

;; p49
(defn cl-lookup [var env denv]
  (if-let [a (get env var)]
    (if-not (= ::dynamic a)
      @a
      ;; lookup in the current dynamic environment
      (if-let [a (get denv var)]
        @a
        ;; default to the global lexical environment
        (lookup var env-global)))
    (wrong "No such binding" var)))

(defn df-do-dynamic-let [bindings body env fenv denv]
  (df-eprogn body
             (special-extend env (map first bindings))
             fenv
             (extend-env denv
                         (map first bindings)
                         (map #(df-evaluate (second %) env fenv denv)
                              bindings))))

(defn df-evaluate-application [f args env fenv denv]
  (match f
    (f :guard symbol?) ((df-lookup f fenv) args denv)
    (['lambda params & body] :seq) (df-eprogn body
                                              (extend-env env params args)
                                              fenv
                                              denv)
    :else (wrong "Incorrect functional term" f)))

(defn df-make-function [variables body env fenv]
  (fn [values denv]
    (df-eprogn body (extend-env env variables values) fenv denv)))

(defn df-do-flet [bindings body env fenv denv]
  (df-eprogn body env
             (extend-env fenv
                         (map first bindings)
                         (map (fn [[_ vars & body]]
                                (df-make-function vars body env fenv))
                              bindings))
             denv))

(defn df-do-let [bindings body env fenv denv]
  (df-eprogn body
             (reduce (fn [env [k e]]
                       (extend-env env
                                   (list k)
                                   (list (df-evaluate e env fenv denv))))
                     env
                     bindings)
             fenv denv))

(defn df-invoke [f args denv]
  (if (fn? f)
    (f args denv)
    (wrong "Not a function" f)))

;; p48-49
(defn df-evaluate [e env fenv denv]
  (match e
    (e :guard symbol?) (cl-lookup e env denv)
    (e :guard #(or (not (coll? %)) (vector? %))) e
    (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
    (['quote x] :seq) x
    (['if p t f] :seq) (if (df-evaluate p env fenv denv)
                         (df-evaluate t env fenv denv)
                         (df-evaluate f env fenv denv))
    (['begin & body] :seq) (df-eprogn body env fenv denv)
    (['set! k v] :seq) (cl-update! k env denv (df-evaluate v env fenv denv))
    (['lambda args & body] :seq) (df-make-function args body env fenv)
    (['let bindings & body] :seq) (df-do-let bindings body env fenv denv)
    (['function (sym :guard symbol?)] :seq) (df-lookup sym fenv)
    (['function (['lambda a & b] :seq)] :seq) (df-make-function a b env fenv)
    (['function & _] :seq) (wrong "Incorrect function" (second e))
    (['flet bindings & body] :seq) (df-do-flet bindings body env fenv denv)
    (['labels defs] :seq) (wrong "TBD: lables") #_(f-do-labels defs env fenv)
    (['dynamic k] :seq) (lookup k denv)
    (['dynamic-let bds & body] :seq) (df-do-dynamic-let bds body env fenv denv)
    (['funcall fex & args] :seq) (df-invoke
                                  (df-evaluate fex env fenv denv)
                                  (df-evlis args env fenv denv)
                                  denv)
    ([f & args] :seq) (df-evaluate-application f
                                               (df-evlis args env fenv denv)
                                               env fenv denv)))
;; tests

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
    (fn [values denv]
      (if (= arity (count values))
        (apply var-value values)
        (wrong "Incorrect arity"
               (list var-name values))))))

(defprimitive* 'car first 1)
(defprimitive* 'cdr rest 1)
(defprimitive* 'cons cons 2)
(defprimitive* '< < 2)
(defprimitive* '> > 2)
(defprimitive* '+ + 2)
(defprimitive* '* * 2)
(defprimitive* list list 2)

(defn eval* [expr]
  (df-evaluate expr env-global fenv-global {}))

(do ;; main-tests
  (is (= 1  (eval* '((lambda (a b) a) 1 2))))
  (is (= 'a (eval* '(if true (quote a) 2))))
  (is (= 5  (eval* '(if false 1 5))))
  (is (= 3  (eval* '(begin 1 2 3))))
  (is (= 9  (eval* '((lambda (a) (set! a 9) a) 5))))
  (is (= () (eval* '(begin))))

  (is (= 9
         (eval* '(flet ((square (x) (* x x)))
                       (square 3)))))

  (is (= 81
         (eval* '(funcall (flet ((square (x) (* x x)))
                                (lambda (x) (square (square x))))
                          3))))

  (is (= '(7 12)
         (eval* '(flet ((wat (a b)
                             (funcall (if (< a b)
                                        (function +)
                                        (function *))
                                      a b)))
                       (cons (wat 3 4) (cons (wat 4 3) '()))))))

  (is (= 9
         (eval* '(flet ((add (a b) (+ a b)))
                       (add 4 5)))))

  (is (= 7
         (eval* '(flet ((bad () (missing-fn 1 2 3)))
                       7)))))

;; late-missing-fn-test
(is (= "WRONG: No such functional binding missing-fn\n"
       (with-out-str
         (eval* '(flet ((bad () (missing-fn 1 2 3)))
                       (bad))))))

(do ;; let-tests
  (is (= '2
         (eval* '(let ((a 2))
                   a))))
  (is (= '3
         (eval* '(let ((a 1))
                   (let ((b 2))
                     (set! a 3))
                   a)))))

;; example p50
(is (= 8
       (eval* '(dynamic-let ((x 2))
                            (+ x                        ; dynamic
                               (let ((x (+              ; lexical
                                         x x)))         ; dynamic
                                 (+ x                   ; lexical
                                    (dynamic x))))))))  ; dynamic
