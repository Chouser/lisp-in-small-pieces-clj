(ns us.chouser.LISP.ch1a-ex3)

(defn wrong [msg & args]
  (apply println "WRONG:" msg (map pr-str args)))

(def env-init ()) ;; env.init in the book

(defn extend-env
  "extend, p29, but with an atom around the values
  This version doesn't support dotted final variable (like `&` or `rest`)"
  [env variables values]
  #_{:post [(do (prn :post-extend variables values %) true)]}
  (cons [variables (atom values)] env))

(defn lookup
  [id env]
  (if-let [[value] (some (fn [[variables *values]]
                           (first
                            (remove nil?
                              (map #(when (= id %1)
                                      [%2])
                                   variables @*values))))
                         env)]
    value
    (wrong "No such binding" id)))

(defn update!
  [id env value]
  (if-let [[value] (some (fn [[variables *values]]
                           (when (some #{id} variables)
                             (swap! *values
                                    (fn [values]
                                      (map (fn [variable old-value]
                                             (if (= id variable)
                                               value
                                               old-value))
                                           variables values)))))
                         env)]
    value
    (wrong "No such binding" id)))


(declare evaluate)

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

(def the-false-value [:false :boolean])

(defn make-function
  "p19"
  [variables body env]
  (fn [values]
    (eprogn body (extend-env env variables values))))

(defn invoke
  "p15"
  [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

(defn evlis
  "p12"
  [exps env]
  (if (seq? exps)
    (let [argument1 (evaluate (first exps) env)]
      (cons argument1 (evlis (next exps) env)))
    ()))

(defn m-let [bindings & body]
  (if (empty? bindings)
    `(~'begin ~@body)
    (let [[sym expr] (first bindings)]
      `((~'lambda (~sym)
                ~(apply m-let (rest bindings) body))
        ~expr))))

(defn evaluate
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
      let (evaluate (apply m-let (rest e)) env)
      lambda (make-function (second e) (nnext e) env)
      #_else (invoke (evaluate (first e) env)
                     (evlis (next e) env)))))

(def env-global env-init)

(defn definitial*
  ([var-name]
   (definitial* var-name 'void))  ;; Why `void`?
  ([var-name var-value]
   #_{:post [(do (prn :def var-name '-- env-global) true)]}
   (alter-var-root #'env-global extend-env (list var-name) (list var-value))))

(defmacro definitial [var-name & args]
  `(definitial* '~var-name ~@args))

(defn defprimitive* [var-name var-value arity]
  (definitial* var-name
    (fn [values]
      (if (= arity (count values))
        (apply var-value values)
        (wrong "Incorrect arity"
               (list var-name values))))))

(defmacro defprimitive [var-name var-value arity]
  `(defprimitive* '~var-name ~var-value ~arity))

(definitial t true)
(definitial f the-false-value)
(definitial null ())  ;; nil isn't read as a symbol, so use null instead of adding an evaluate rule

(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)

(defprimitive cons cons 2)
(defprimitive car first 1)
(defprimitive set-cdr! reset! 2)  ;; probably not the correct definition for Clojure
(defprimitive + + 2)
(defprimitive eq? = 2)
(defprimitive < < 2)
(defprimitive list list 2)

(defn chapter1-scheme []
  (let [in (read)
        _ (prn in)
        out (evaluate in env-global)]
    (prn '=> out)
    (when (not= 'exit out)
      (recur))))

(assert (= 1  (evaluate '((lambda (a b) a) 1 2)        env-global)))
(assert (= 'a (evaluate '(if true (quote a) 2)         env-global)))
(assert (= 5  (evaluate '(if f 1 5)                env-global)))
(assert (= 3  (evaluate '(begin 1 2 3)                 env-global)))
(assert (= 9  (evaluate '((lambda (a) (set! a 9) a) 5) env-global)))
(assert (= 3   (evaluate '(begin 1 2 3) env-global)))
(assert (= 813 (evaluate '(begin) env-global)))
(assert (= 'a (evaluate '(if true (quote a) 2)         env-global)))
(assert (= 5  (evaluate `(if ~the-false-value 1 5)     env-global)))
(assert (= 7 (evaluate '(+ 4 3)                  env-global)))
(assert (= '(a 3) (evaluate '(list (quote a) 3)  env-global)))

(def p20-example
  '(let ((y 0)
         (foo (lambda (x) (list x y)))
         (bar (lambda (y) (foo 1991))))
        (list (bar 100) (foo 3))))

(assert (= '((1991 0) (3 0)) (evaluate p20-example env-global)))
(assert (= () (evaluate 'null env-global)))

(assert "WRONG: No such binding a"
        (with-out-str
          (evaluate
           '((lambda (a)
                     ((lambda (b)
                              (list a b))
                      (+ 2 a)))
             1)
           env-global)))

(assert (= '(1 2)
           (evaluate
            '(((lambda (a)
                       (lambda (b) (list a b)))
               1)
              2)
            env-global)))

(assert (= '(7 true)
           (evaluate '(cons (car (cons (+ 2 5) 'nil))
                            (cons (< 1 2) 'nil))
                     env-global)))
