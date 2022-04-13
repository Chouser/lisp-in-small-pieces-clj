(ns us.chouser.LISP.ch1a)

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(defn lookup
  "p13"
  [id env]
  (if (seq? env)
    (let [entry (first env)]
      (if (= (first entry) id)
        @(second entry)
        (recur id (next env))))
    (wrong "No such binding" id)))

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

(declare evaluate)

(defn eprogn
  "Like Clojure's `do` for our interpreter. p9"
  [exps env]
  (if (seq exps)
    (if (next exps)
      (do
        (evaluate (first exps) env)
        (recur (next exps) env))
      (evaluate (first exps) env))
    ()))

(def env-init ()) ;; env.init in the book

(defn extend-env
  "extend, p14.
  This version doesn't support dotted final variable (like `&` or `rest`)"
  [env variables values]
  (cond
    (seq? variables) (if (seq? values)
                       (cons (list (first variables) (atom (first values)))
                             (extend-env env (next variables) (next values)))
                       (wrong "Too less values"))
    (empty? variables) (if (empty? values)
                         env
                         (wrong "Too much values"))))

(defn invoke
  "p15"
  [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defn make-function
  "p16"
  [variables body env]
  (fn [values]
    (eprogn body (extend-env env-init variables values))))

(defn evlis
  "p12"
  [exps env]
  (if (seq? exps)
    (let [argument1 (evaluate (first exps) env)]
      (cons argument1 (evlis (next exps) env)))
    ()))

(defn evaluate
  "p7"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (evaluate (nth e 1) env)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      begin (eprogn (next e) env)
      set! (update! (nth e 1) env (evaluate (nth e 2) env))
      lambda (make-function (second e) (nnext e) env)
      #_else (invoke (evaluate (first e) env)
                     (evlis (next e) env)))))

(assert (= 1  (evaluate '((lambda (a b) a) 1 2)        env-init)))
(assert (= 'a (evaluate '(if true (quote a) 2)         env-init)))
(assert (= 5  (evaluate '(if false 1 5)                env-init)))
(assert (= 3  (evaluate '(begin 1 2 3)                 env-init)))
(assert (= 9  (evaluate '((lambda (a) (set! a 9) a) 5) env-init)))
(assert (= () (evaluate '(begin)                       env-init)))

;;;;;
;; Alternatives
;;

(def empty-begin 813)

(defn eprogn
  "p10. Why must empty `begin` return an empty list?"
  [exps env]
  (if (seq exps)
    (if (next exps)
      (do
        (evaluate (first exps) env)
        (recur (next exps) env))
      (evaluate (first exps) env))
    empty-begin))

(assert (= 3   (evaluate '(begin 1 2 3) env-init)))
(assert (= 813 (evaluate '(begin) env-init)))


(def the-false-value [:false :boolean])

(defn evaluate
  "p9. Why use the host (implementing) language's booleans in ours?"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (not= (evaluate (nth e 1) env)
                   the-false-value)
           (evaluate (nth e 2) env)
           (evaluate (nth e 3) env))
      begin (eprogn (next e) env)
      set! (update! (nth e 1) env (evaluate (nth e 2) env))
      lambda (make-function (second e) (nnext e) env)
      #_else (invoke (evaluate (first e) env)
                     (evlis (next e) env)))))

(assert (= 'a (evaluate '(if true (quote a) 2)         env-init)))
(assert (= 5  (evaluate `(if ~the-false-value 1 5)     env-init)))
