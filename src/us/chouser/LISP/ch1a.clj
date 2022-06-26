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
  #_{:post [(do (prn :post-extend variables values %) true)]}
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


;; What if functions can see a (static?) global environment?

(def env-global
  (extend-env ()
              '(list +)
              (list identity
                    #(apply + %))))

(defn make-function
  "p16"
  [variables body env]
  (fn [values]
    (eprogn body (extend-env env-global variables values))))

(assert (= 7 (evaluate '(+ 4 3)                  env-global)))
(assert (= '(a 3) (evaluate '(list (quote a) 3)  env-global)))

(assert "WRONG: No such binding a"
        (with-out-str
          (evaluate
           '((lambda (a)
                     ((lambda (b)
                              (list a b))
                      (+ 2 a)))
             1)
           env-global)))


;; Dynamic scope

(declare d-evaluate)

(defn d-eprogn
  "Dynamic version of eprogn somehow never mentioned in chapter 1?"
  [exps env]
  (if (seq exps)
    (if (next exps)
      (do
        (d-evaluate (first exps) env)
        (recur (next exps) env))
      (d-evaluate (first exps) env))
    ()))

(defn d-evlis
  "Dynamic version of evlis also not mentioned in chapter 1?"
  [exps env]
  (if (seq? exps)
    (let [argument1 (d-evaluate (first exps) env)]
      (cons argument1 (d-evlis (next exps) env)))
    ()))

(defn d-invoke
  "p18"
  [f args env]
  (if (fn? f)
    (f args env)
    (wrong "Not a function" f)))

(defn d-make-function
  "p18"
  [variables body def-env]
  (fn [values current-env]
    (let [ext (extend-env current-env variables values)]
      (d-eprogn body ext))))

(def d-env-global
  (extend-env ()
              '(list +)
              (list (fn [args env] args)
                    (fn [args env] (apply + args)))))

(defn d-evaluate
  "p17"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (d-evaluate (nth e 1) env)
           (d-evaluate (nth e 2) env)
           (d-evaluate (nth e 3) env))
      begin (d-eprogn (next e) env)
      set! (update! (nth e 1) env (d-evaluate (nth e 2) env))
      lambda (d-make-function (second e) (nnext e) env)
      #_else (d-invoke (d-evaluate (first e) env)
                       (d-evlis (next e) env)
                       env))))

(assert (= '(1 3)
           (d-evaluate
            '((lambda (a)
                      ((lambda (b)
                               (list a b))
                       (+ 2 a)))
              1)
            d-env-global)))

(assert "WRONG: No such binding a"
        (with-out-str
          (d-evaluate
           '(((lambda (a)
                      (lambda (b) (list a b)))
              1)
             2)
           d-env-global)))

;; Proper lexical scoping

(defn make-function
  "p19"
  [variables body env]
  (fn [values]
    (eprogn body (extend-env env variables values))))

(assert (= '(1 2)
           (evaluate
            '(((lambda (a)
                       (lambda (b) (list a b)))
               1)
              2)
            env-global)))



(def env-global
  (extend-env ()
              '(list + define)
              (list identity
                    #(apply + %)
                    #())))

;; No definition for `define` is given in the book, but I think it implies a
;; mutable environment which we don't have (only mutable vars within the env).
;; So, here's a kind of `let` macro I can use to set up the next example from
;; the book.
(defn m-let [bindings & body]
  (if (empty? bindings)
    `(~'begin ~@body)
    (let [[sym expr] (first bindings)]
      `((~'lambda (~sym)
                ~(apply m-let (rest bindings) body))
        ~expr))))

(def p20-example
  (m-let '((y 0)
           (foo (lambda (x) (list x y)))
           (bar (lambda (y) (foo 1991))))
         '(list (bar 100) (foo 3))))

(assert (= '((1991 0)   (3 0)) (evaluate   p20-example env-global)))
(assert (= '((1991 100) (3 0)) (d-evaluate p20-example d-env-global)))


(defn d-evaluate
  "p20"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (d-evaluate (nth e 1) env)
           (d-evaluate (nth e 2) env)
           (d-evaluate (nth e 3) env))
      begin (d-eprogn (next e) env)
      set! (update! (nth e 1) env (d-evaluate (nth e 2) env))
      let (d-evaluate (apply m-let (rest e)) env)
      lambda (d-make-function (second e) (nnext e) env)
      #_else (d-invoke (d-evaluate (first e) env)
                       (d-evlis (next e) env)
                       env))))

(assert (= '((1991 100) (3 0))
           (d-evaluate '(let ((y 0)
                              (foo (lambda (x) (list x y)))
                              (bar (lambda (y) (foo 1991))))
                          (list (bar 100) (foo 3)))
                       d-env-global)))

(assert (= '(1 3)
           (d-evaluate '(let ((a 1))
                          ((let ((a 2))
                             (lambda (b)
                                     (list a b)))
                           3))
                       d-env-global)))

(defn d-make-closure
  "p22"
  [f env]
  (fn [values current-env]
    (f values env)))

(defn d-evaluate
  "p21"
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (d-evaluate (nth e 1) env)
           (d-evaluate (nth e 2) env)
           (d-evaluate (nth e 3) env))
      begin (d-eprogn (next e) env)
      set! (update! (nth e 1) env (d-evaluate (nth e 2) env))
      let (d-evaluate (apply m-let (rest e)) env)

      function ; Syntax: (function (lambda variables body))
      (let [f (second e)
            fun (d-make-function (second f) (nnext f) env)]
        (d-make-closure fun env))

      lambda (d-make-function (second e) (nnext e) env)
      #_else (d-invoke (d-evaluate (first e) env)
                       (d-evlis (next e) env)
                       env))))

(assert (= '(2 3)
           (d-evaluate '(let ((a 1))
                          ((let ((a 2))
                             (function (lambda (b)
                                               (list a b))))
                           3))
                       d-env-global)))

;; Since I'm using Clojure's reader, there's no "place" share between multiple
;; uses of a symbol on which to store properties, like Scheme's `putprop` does.
;; So instead I'll simulate that by have a my own map of symbols to `apval`
;; properties in a mutable place:
(def *apval (atom {'list (fn [args env] args)
                   '+ (fn [args env] (apply + args))}))

(declare s-evaluate)

(defn s-lookup
  "p24"
  [id env]
  (get @*apval id))

(defn s-update!
  "p24"
  [id env value]
  (swap! *apval assoc id value))

(defn s-eprogn
  [exps env]
  (if (seq exps)
    (if (next exps)
      (do
        (s-evaluate (first exps) env)
        (recur (next exps) env))
      (s-evaluate (first exps) env))
    empty-begin))

(defn s-make-function
  "p24"
  [variables body env]
  (fn [values current-env]
    (let [old-bindings (mapv (fn [var val]
                               (let [old-value (get @*apval var)]
                                 (swap! *apval assoc var val)
                                 [var old-value]))
                             variables
                             values)
          result (s-eprogn body current-env)]
      (run! (fn [[var val]] (swap! *apval assoc var val))
            old-bindings)
      result)))

(defn s-evaluate
  "Shallow dynamic binding."
  [e env]
  (if-not (seq? e)
    (cond
      (symbol? e) (s-lookup e env)
      (or (number? e) (string? e) (char? e) (boolean? e) (vector? e)) e
      :else (wrong "Cannot evaluate" e))
    (case (first e)
      quote (second e)
      if (if (not= (s-evaluate (nth e 1) env)
                   the-false-value)
           (s-evaluate (nth e 2) env)
           (s-evaluate (nth e 3) env))
      begin (last (map #(s-evaluate % env) (next e)))
      set! (s-update! (nth e 1) env (s-evaluate (nth e 2) env))
      let (s-evaluate (apply m-let (rest e)) env)
      lambda (s-make-function (second e) (nnext e) env)
      #_else (d-invoke (s-evaluate (first e) env)
                       (doall (map #(s-evaluate % env) (next e)))
                       env))))

(assert (= '((1991 100) (3 0))
           (s-evaluate
            '(let ((y 0)
                   (foo (lambda (x) (list x y)))
                   (bar (lambda (y) (foo 1991))))
                  (list (bar 100) (foo 3)))
            env-global)))


;; section 1.7 p 25

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

(assert (= () (evaluate 'null env-global)))

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

(assert (= '(7 true)
           (evaluate '(cons (car (cons (+ 2 5) 'nil))
                            (cons (< 1 2) 'nil))
                     env-global)))

(defn chapter1-scheme []
  (let [in (read)
        out (evaluate in env-global)]
    (prn in)
    (prn '=> out)
    (when (not= 'exit out)
      (recur))))
