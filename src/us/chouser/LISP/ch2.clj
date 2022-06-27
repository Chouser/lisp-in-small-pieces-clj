(ns us.chouser.LISP.ch2)

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

(defn evaluate-application
  "p34, p35"
  [f args env fenv]
  (cond
    (symbol? f)
    ((lookup f fenv) args)

    (and (seq? f) (= (first f) 'lambda))
    (let [[_ params & body] f]
      (f-eprogn body
                (extend-env env params args)
                fenv))

    :else (wrong "Incorrect functional term" f)))

(defn f-evaluate
  "p33. A Lisp2"
  [e env fenv]
  (if-not (seq? e)
    (cond
      (symbol? e) (lookup e env)
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
      flet (f-eprogn (nnext e)
                     env
                     (extend-env fenv
                                 (map first (second e))
                                 (map (fn [[_ vars & body]]
                                        (f-make-function vars body env fenv))
                                      (second e))))
      function (cond
                 (symbol? (second e)) (lookup (second e) fenv)
                 :else (wrong "Incorrect function" (second e)))
      #_else (evaluate-application (first e)
                                   (f-evlis (next e) env fenv)
                                   env
                                   fenv))))

(let [env ()
      fenv ()]
  (assert (= 1  (f-evaluate '((lambda (a b) a) 1 2)        env fenv)))
  (assert (= 'a (f-evaluate '(if true (quote a) 2)         env fenv)))
  (assert (= 5  (f-evaluate '(if false 1 5)                env fenv)))
  (assert (= 3  (f-evaluate '(begin 1 2 3)                 env fenv)))
  (assert (= 9  (f-evaluate '((lambda (a) (set! a 9) a) 5) env fenv)))
  (assert (= () (f-evaluate '(begin)                       env fenv))))

(def env-global ())
(def fenv-global ())

(defn definitial-function*
  ([fn-name] (definitial-function* fn-name 'void))
  ([fn-name value]
   (alter-var-root #'fenv-global extend-env (list fn-name) (list value))))

(defn defprimitive*
  [fn-name value arity]
  (definitial-function* fn-name
    (fn [values]
      (if (= arity (count values))
        (apply value values)
        (wrong "Incorrct arity" (list 'name values))))))

(defprimitive* 'car first 1)
(defprimitive* 'cons cons 2)
(defprimitive* '< < 2)
(defprimitive* '> > 2)
(defprimitive* '+ + 2)
(defprimitive* '* * 2)

(assert (= 9
           (f-evaluate '(flet ((square (x) (* x x)))
                              (square 3))
                       env-global
                       fenv-global)))

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

(assert (= 81
           (f-evaluate '(funcall (flet ((square (x) (* x x)))
                                       (lambda (x) (square (square x))))
                                 3)
                       env-global
                       fenv-global)))

(assert (= '(7 12)
           (f-evaluate '(flet ((wat (a b)
                                    (funcall2 (if (< a b)
                                                (function +)
                                                (function *))
                                              a b)))
                              (cons (wat 3 4) (cons (wat 4 3) '())))
                       env-global
                       fenv-global)))

(defn a-certain-lisp2 []
  (let [string (read)]
    (when-not (= 'exit string)
      (prn (f-evaluate string env-global fenv-global))
      (recur))))
