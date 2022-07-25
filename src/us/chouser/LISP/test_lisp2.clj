(ns us.chouser.LISP.test-lisp2
  (:require  [clojure.test :as t :refer [is]]))

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(defn apply-tests [{:keys [env
                           fenv
                           extend-env
                           invoke
                           f-evaluate]}]

  (is (= 1  (f-evaluate '((lambda (a b) a) 1 2)        env fenv)))
  (is (= 'a (f-evaluate '(if true (quote a) 2)         env fenv)))
  (is (= 5  (f-evaluate '(if false 1 5)                env fenv)))
  (is (= 3  (f-evaluate '(begin 1 2 3)                 env fenv)))
  (is (= 9  (f-evaluate '((lambda (a) (set! a 9) a) 5) env fenv)))
  (is (= () (f-evaluate '(begin)                       env fenv)))

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
          (wrong "Incorrect arity" 'funcall)))
      3)

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
                       @*env @*fenv)))))
