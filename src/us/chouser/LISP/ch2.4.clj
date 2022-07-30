(ns us.chouser.LISP.ch2.4
  "A Lisp2 with an f-lookup that always returns in constant time. p41-42"
  (:require [clojure.test :refer [is]]
            [us.chouser.LISP.test-lisp2 :as t]))

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(declare f-evaluate)

;; These two are for p.33, but implenented in idiomatic Clojure, for variety.
;; See ch1a.clj for a stricter rendering
(defn f-evlis [exps env fenv]
  (doall (map #(f-evaluate % env fenv) exps)))

(defn f-eprogn
  [exps env fenv]
  (if (empty? exps)
    ()
    (last (map #(f-evaluate % env fenv) exps))))

(defn lookup
  "p13"
  [id env]
  (if (seq? env)
    (let [entry (first env)]
      (if (= (first entry) id)
        @(second entry)
        (recur id (next env))))
    (wrong "No such binding" id)))

(defn f-lookup [id fenv]
  (if (seq? fenv)
    (if (= (ffirst fenv) id)
      @(second (first fenv))
      (recur id (next fenv)))
    (fn [values]
      (wrong "No such functional binding" id))))

(defn update!
  "p13"
  [id env value]
  (if (seq? env)
    (let [entry (first env)]
      (if (= (first entry) id)
        (do (reset! (second entry) value)
            value)
        (recur id (next env) value)))
    (wrong "No such binding" id)))

(defn extend-env
  "extend, p14.
  This version doesn't support dotted final variable (like `&` or `rest`)"
  [env variables values]
  #_{:post [(do (prn :post-extend variables values %) true)]}
  (cond
    (seq? variables) (if (seq? values)
                       (cons (list (first variables) (atom (first values)))
                             (extend-env env (next variables) (next values)))
                       (wrong "Too less values"))
    (empty? variables) (if (empty? values)
                         env
                         (wrong "Too much values"))))

(defn f-make-function
  "p34"
  [variables body env fenv]
  (fn [values]
    (f-eprogn body (extend-env env variables values) fenv)))

(defn invoke
  "p15"
  [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defn evaluate-application [f args env fenv]
  (cond
    (symbol? f)
    ((f-lookup f fenv) args)

    (and (seq? f) (= (first f) 'lambda))
    (let [[_ params & body] f]
      (f-eprogn body
                (extend-env env params args)
                fenv))

    :else (wrong "Incorrect functional term" f)))

(defn f-evaluate [e env fenv]
  (if-not (seq? e)
    (cond (symbol? e) (lookup e env)
          (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
          :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (f-evaluate (nth e 1) env fenv)
           (f-evaluate (nth e 2) env fenv)
           (f-evaluate (nth e 3) env fenv))
      begin (f-eprogn (next e) env fenv)
      set! (update! (nth e 1)
                    env
                    (f-evaluate (nth e 2) env fenv))
      lambda (f-make-function (second e) (nnext e) env fenv)
      let (let [[_let bindings & body] e]
            (if (empty? bindings)
              (f-eprogn body env fenv)
              (let [[sym expr] (first bindings)]
                (f-evaluate `((~'lambda (~sym)
                               (~'let ~(rest bindings)
                                ~@body))
                              ~expr)
                            env fenv))))
      function (cond
                 (symbol? (second e))
                 , (f-lookup (second e) fenv)
                 (and (seq? (second e)) (= (first (second e)) 'lambda))
                 , (f-make-function (second (second e))
                                    (nnext (second e))
                                    env fenv)
                 :else (wrong "Incorrect function" (second e)))
      flet (f-eprogn (nnext e)
                     env
                     (extend-env fenv
                                 (map first (second e))
                                 (map (fn [[_ vars & body]]
                                        (f-make-function vars body env fenv))
                                      (second e))))
      labels (let [new-fenv (extend-env fenv
                                        (map first (second e))
                                        (map (constantly 'void) (second e)))]
               (doseq [def (second e)]
                 (update! (first def)
                          new-fenv
                          (f-make-function (second def)
                                           (nnext def)
                                           env new-fenv))))
      #_else (evaluate-application (first e)
                                   (f-evlis (next e) env fenv)
                                   env
                                   fenv))))

(doto (t/kwmap extend-env invoke f-evaluate
               :env () :fenv ())
  t/main-tests
  t/late-missing-fn-test
  t/let-tests)
