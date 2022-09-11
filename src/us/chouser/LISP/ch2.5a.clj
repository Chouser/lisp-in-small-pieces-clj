(ns us.chouser.LISP.ch2.5a
  (:require [clojure.core.reducers :as r]
            [clojure.core.match :refer [match]]
            [us.chouser.spread :refer [k.]]
            [clojure.test :as t :refer [is]]))

;; Adding another environment (this time the dynamic variables `denv`), again
;; broke the interfaces of a bunch of functions, including my whole test-lisp2
;; suite. Many of the functions pass all three environments together, so if they
;; had been bundled into a map or object, adding another would have broken much
;; less -- I wonder if this would have been a net win.

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

(defn df-do-dynamic-let [bindings body env fenv denv]
  (df-eprogn body env fenv
             (extend-env denv
                         (map first bindings)
                         (map #(df-evaluate % env fenv denv) bindings))))

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

(defn df-evaluate [e env fenv denv]
  #_(prn :eval e)
  (match e
    (e :guard symbol?) (lookup e env)
    (e :guard #(or (not (coll? %)) (vector? %))) e
    (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
    (['quote x] :seq) x
    (['if p t f] :seq) (if (df-evaluate p env fenv denv)
                         (df-evaluate t env fenv denv)
                         (df-evaluate f env fenv denv))
    (['begin & body] :seq) (df-eprogn body env fenv denv)
    (['set! k v] :seq) (update! k env (df-evaluate v env fenv denv))
    (['lambda args & body] :seq) (df-make-function args body env fenv)
    (['let bindings & body] :seq) (df-do-let bindings body env fenv denv)
    (['function (sym :guard symbol?)] :seq) (df-lookup sym fenv)
    (['function (['lambda a & b] :seq)] :seq) (df-make-function a b env fenv)
    (['function & _] :seq) (wrong "Incorrect function" (second e))
    (['flet bindings & body] :seq) (df-do-flet bindings body env fenv denv)
    (['labels defs] :seq) (wrong "TBD: lables") #_(f-do-labels defs env fenv)
    (['dynamic k] :seq) (lookup k denv)
    (['dynamic-set! k v] :seq) (update! k denv (df-evaluate v env fenv denv))
    (['dynamic-let bds & body] :seq) (df-do-dynamic-let bds body env fenv denv)
    (['funcall fex & args] :seq) (df-invoke
                                  (df-evaluate fex env fenv denv)
                                  (df-evlis args env fenv denv)
                                  denv)
    ([f & args] :seq) (df-evaluate-application f
                                               (df-evlis args env fenv denv)
                                               env fenv denv)))

;;;; Tests:
(let [*env (atom {})
      *fenv (atom {})

      defprimitive*
      (fn [fn-name value arity]
        (swap! *fenv extend-env (list fn-name)
               (list (fn [values denv]
                       (if (= arity (count values))
                         (apply value values)
                         (wrong "Incorrct arity" (list 'name values)))))))]

  (defprimitive* 'car first 1)
  (defprimitive* 'cdr rest 1)
  (defprimitive* 'cons cons 2)
  (defprimitive* '< < 2)
  (defprimitive* '> > 2)
  (defprimitive* '+ + 2)
  (defprimitive* '* * 2)

  #_[*env *fenv]
  (def *env *env)
  (def *fenv *fenv)
  (def denv {}))

(do ;; main-tests
  (is (= 1  (df-evaluate '((lambda (a b) a) 1 2)        @*env @*fenv denv)))
  (is (= 'a (df-evaluate '(if true (quote a) 2)         @*env @*fenv denv)))
  (is (= 5  (df-evaluate '(if false 1 5)                @*env @*fenv denv)))
  (is (= 3  (df-evaluate '(begin 1 2 3)                 @*env @*fenv denv)))
  (is (= 9  (df-evaluate '((lambda (a) (set! a 9) a) 5) @*env @*fenv denv)))
  (is (= () (df-evaluate '(begin)                       @*env @*fenv denv)))

  (is (= 9
         (df-evaluate '(flet ((square (x) (* x x)))
                             (square 3))
                      @*env @*fenv denv)))

  (is (= 81
         (df-evaluate '(funcall (flet ((square (x) (* x x)))
                                      (lambda (x) (square (square x))))
                                3)
                      @*env @*fenv denv)))

  (is (= '(7 12)
         (df-evaluate '(flet ((wat (a b)
                                   (funcall (if (< a b)
                                              (function +)
                                              (function *))
                                             a b)))
                             (cons (wat 3 4) (cons (wat 4 3) '())))
                      @*env @*fenv denv)))

  (is (= 9
         (df-evaluate '(flet ((add (a b) (+ a b)))
                             (add 4 5))
                      @*env @*fenv denv)))

  (is (= 7
         (df-evaluate '(flet ((bad () (missing-fn 1 2 3)))
                             7)
                      @*env @*fenv denv))))

;; late-missing-fn-test
(is (= "WRONG: No such functional binding missing-fn\n"
       (with-out-str
         (df-evaluate '(flet ((bad () (missing-fn 1 2 3)))
                             (bad))
                      @*env @*fenv denv))))

(do ;; let-tests
  (is (= '2
         (df-evaluate '(let ((a 2))
                         a)
                      @*env @*fenv denv)))
  (is (= '3
         (df-evaluate '(let ((a 1))
                         (let ((b 2))
                           (set! a 3))
                         a)
                      @*env @*fenv denv))))
