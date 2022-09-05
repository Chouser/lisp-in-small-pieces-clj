(ns us.chouser.LISP.test-lisp2
  (:require [clojure.test :as t :refer [is]]))

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(defn mk-envs [{:keys [env fenv extend-env invoke]}]
  (let [*env (atom env)
        *fenv (atom fenv)

        defprimitive*
        (fn [fn-name value arity]
          (swap! *fenv extend-env (list fn-name)
                 (list (fn [values]
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

    (defprimitive* 'funcall
      (fn [& args]
        (if (> (count args) 1)
          (invoke (first args) (rest args))
          (wrong "Incorrect arity" 'funcall)))
      2)

    (defprimitive* 'funcall2
      (fn [& args]
        (if (> (count args) 1)
          (invoke (first args) (rest args))
          (wrong "Incorrect arity" 'funcall2)))
      3)
    [*env *fenv]))

(defn main-tests [{:keys [env fenv extend-env invoke f-evaluate] :as lisp}]

  (is (= 1  (f-evaluate '((lambda (a b) a) 1 2)        env fenv)))
  (is (= 'a (f-evaluate '(if true (quote a) 2)         env fenv)))
  (is (= 5  (f-evaluate '(if false 1 5)                env fenv)))
  (is (= 3  (f-evaluate '(begin 1 2 3)                 env fenv)))
  (is (= 9  (f-evaluate '((lambda (a) (set! a 9) a) 5) env fenv)))
  (is (= () (f-evaluate '(begin)                       env fenv)))

  (let [[*env *fenv] (mk-envs lisp)]
    (is (= 9
           (f-evaluate '(flet ((square (x) (* x x)))
                              (square 3))
                       @*env @*fenv)))

    (is (= 81
           (f-evaluate '(funcall (flet ((square (x) (* x x)))
                                       (lambda (x) (square (square x))))
                                 3)
                       @*env @*fenv)))

    (is (= '(7 12)
           (f-evaluate '(flet ((wat (a b)
                                    (funcall2 (if (< a b)
                                                (function +)
                                                (function *))
                                              a b)))
                              (cons (wat 3 4) (cons (wat 4 3) '())))
                       @*env @*fenv)))

    (is (= 9
           (f-evaluate '(flet ((add (a b) (+ a b)))
                              (add 4 5))
                       @*env @*fenv)))

    (is (= 7
           (f-evaluate '(flet ((bad () (missing-fn 1 2 3)))
                              7)
                       @*env @*fenv)))))

(defn late-missing-fn-test [{:keys [f-evaluate] :as lisp}]
  (let [[*env *fenv] (mk-envs lisp)]
    (is (= "WRONG: No such functional binding missing-fn\n"
           (with-out-str
             (f-evaluate '(flet ((bad () (missing-fn 1 2 3)))
                                (bad))
                         @*env @*fenv))))))

(defn let-tests [{:keys [env fenv extend-env invoke f-evaluate] :as lisp}]
  (let [[*env *fenv] (mk-envs lisp)]
    (is (= '2
           (f-evaluate '(let ((a 2))
                          a)
                       @*env @*fenv)))
    (is (= '3
           (f-evaluate '(let ((a 1))
                          (let ((b 2))
                            (set! a 3))
                          a)
                       @*env @*fenv)))))

(defn extend-env-tests [{:keys [env extend-env lookup] :as lisp}]
  (let [env (extend-env env '(a b c) '(1 2 3))]
    (is (= 1 (lookup 'a env)))
    (is (= 3 (lookup 'c env))))
  (let [env (extend-env env '(a b & c) '(1 2 3 4))]
    (is (= 1 (lookup 'a env)))
    (is (= '(3 4) (lookup 'c env))))
  (let [env (extend-env env '(a b & c) '(1 2 3))]
    (is (= 1 (lookup 'a env)))
    (is (= '(3) (lookup 'c env))))
  (let [env (extend-env env '(a b & c) '(1 2))]
    (is (= 1 (lookup 'a env)))
    (is (= '() (lookup 'c env))))
  (let [env (extend-env env '(& a) '(1 2))]
    (is (= '(1 2) (lookup 'a env)))
    (is (= "WRONG: No such binding c\n"
           (with-out-str (lookup 'c env)))))
  (let [env (extend-env env '(& a) '())]
    (is (= '() (lookup 'a env))))
  (is (= "WRONG: Too less values\n"
         (with-out-str (extend-env env '(a b c) '(1 2)))))
  (is (= "WRONG: Too less values\n"
         (with-out-str (extend-env env '(a b & c) '(1)))))
  (is (= "WRONG: Too much values\n"
         (with-out-str (extend-env env '(a b) '(1 2 3))))))
