(ns us.chouser.LISP.ch3-2-trampoline
  ""
  (:require [clojure.test :refer [is]]
            [clojure.core.match :refer [match]]))

(defn wrong [msg & args]
  (throw (ex-info (apply str "WRONG: " msg " "
                         (interpose " " (map pr-str args)))
                  (reduce merge {} (map meta args)))))

(def empty-begin (reify Object (toString [_] "empty-begin")))
(def the-false-value (reify Object (toString [_] "the-false-value")))

(defn extend-env [env variables values]
  (let [[normal-vars [_ rest-var]] (split-with #(not= '& %) variables)
        [normal-vals rest-val] (split-at (count normal-vars) values)]
    (cond
      (< (count normal-vals) (count normal-vars)) (wrong "Too less values")
      (and (seq rest-val) (not rest-var)) (wrong "Too much values")
      :else (-> env
                (merge (zipmap normal-vars (map atom normal-vals)))
                (cond-> rest-var (assoc rest-var (atom rest-val)))))))

#_
(defn invoke [f args]
  (if (fn? f)
    (f args)
    (wrong "Not a function" f)))

;; (define-generic (invoke (f) v* r k)), p89
(defmulti invoke (fn [f v* env k] (:type f)))
(defmulti resume (fn [k v] (:type k)))

(defmethod invoke :default [f v* env k]
  (wrong "not a function" f env k))

(defmethod resume :default [k v]
  (wrong "Unknown continuation" k))


(defn evaluate [expr env k]
  {:type ::eval, :expr expr, :env env, :k k})

;; (define-class if-cont continuation (et ef r)), p90
(derive ::if-cont ::continutation)
;; (define-method (resume (k if-cont) v) ...)
(defmethod resume ::if-cont [k v]
  (evaluate (if (not= v the-false-value) (:et k) (:ef k))
            (:env k)
            (:k k)))
(defn evaluate-if [ec et ef env k]
  (evaluate ec env {:type ::if-cont
                    :et et
                    :ef ef
                    :k k
                    :env env}))


(derive ::begin-cont ::continutation)
(defn evaluate-begin [exps env k]
  (cond
    (empty? exps) (resume k empty-begin)
    (empty? (rest exps)) (evaluate (first exps) env k)
    :else (evaluate (first exps) env {:type ::begin-cont
                                      :exps exps
                                      :k k
                                      :env env})))
(defmethod resume ::begin-cont [k v]
  (evaluate-begin (-> k :exps rest)
                  (:env k)
                  (:k k)))


(def the-uninitialized-marker (reify Object (toString [_] "uninitialized")))

;; p91
(defn lookup [id env k]
  (if-let [a (get env id)]
    (let [v @a]
      (if (= the-uninitialized-marker v)
        (wrong "Uninitialized binding" id)
        (resume k v)))
    (wrong "No such binding" id)))

(defn update! [env id k value]
  (if-let [a (get env id)]
    (resume k (reset! a value))
    (wrong "No such binding" id)))

(derive ::set!-cont ::continutation)
(defn evaluate-set! [n e env k]
  (evaluate e env {:type ::set!-cont
                   :n n
                   :k k
                   :env env}))
(defmethod resume ::set!-cont [k v]
  (update! (:env k) (:n k) (:k k) v))


(derive ::function ::value)
(defn evaluate-lambda [args body env k]
  (resume k {:type ::function, :args args, :body body, :env env}))
(defmethod invoke ::function [f v* _env k]
  (let [env (extend-env (:env f) (:args f) v*)]
    (evaluate-begin (:body f) env k)))


(derive ::evfun-cont ::continuation) ;; p93
(derive ::apply-cont ::continuation)
(derive ::argument-cont ::continuation)
(derive ::gather-cont ::continuation)
(defn evaluate-arguments [args env k]
  (if (seq args)
    (evaluate (first args) env {:type ::argument-cont
                                :args args :k k :env env})
    (resume k ())))
(defn evaluate-application [f args env k]
  (evaluate f env {:type ::evfun-cont, :args args, :k k, :env env}))
(defmethod resume ::evfun-cont [k f]
  (evaluate-arguments (:args k) (:env k) {:type ::apply-cont
                                          :f f
                                          :k (:k k)
                                          :env (:env k)}))
(defmethod resume ::argument-cont [k v]
  (evaluate-arguments (rest (:args k))
                      (:env k)
                      {:type ::gather-cont
                       :v v
                       :k (:k k)}))
(defmethod resume ::gather-cont [k args]
  (resume (:k k) (cons (:v k) args)))
(defmethod resume ::apply-cont [k v]
  (invoke (:f k) v (:env k) (:k k)))

;; p61: `let` with simultaneous bindings, supporting uninitialized variables
#_
(defn do-let [bindings body env k]
  (eprogn body
          (extend-env env
                      (map #(if (symbol? %) % (first %)) bindings)
                      (map #(if (symbol? %)
                              the-uninitialized-marker
                              (evaluate (second %) env k))
                           bindings))
          k))

#_
(defn do-letrec [bindings body env k]
  (let [temps (map #(symbol (str "_temp_" %)) (range (count bindings)))]
    (evaluate
     `(~'let ~(map first bindings)
       (~'let ~(map list temps (map second bindings))
        ~@(map #(list 'set! %1 %2) (map first bindings) temps)
        ~@body))
     env
     k)))

#_
(defn evlis [exps env k]
  (mapv #(evaluate % env k) exps))

(defn dissoc-env [k]
  (when k
    (-> k
        (dissoc :env)
        (update :k dissoc-env))))

(defn eval-loop [e env k]
  #_(prn :e e :k (dissoc-env k))
  (let [z (match e
                 (e :guard symbol?) (lookup e env k)
                 (e :guard #(or (not (coll? %)) (vector? %))) (resume k e)
                 (e :guard #(not (seq? %))) (wrong "Cannot evaluate" e)
                 (['quote x] :seq) (resume k x)
                 (['if p t f] :seq) (evaluate-if p t f env k)
                 (['begin & body] :seq) (evaluate-begin body env k)
                 (['set! variable value] :seq) (evaluate-set! variable value env k)
                 (['lambda args & body] :seq) (evaluate-lambda args body env k)
                 ;;(['let bindings & body] :seq) (do-let bindings body env k)
                 ;;(['letrec bindings & body] :seq) (do-letrec bindings body env k)
                 ([f & args] :seq) (evaluate-application f args env k))]
    (if (= :done (:type z))
      (:value z)
      (recur (:expr z) (:env z) (:k z)))))

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
    {:type ::primitive
     :name var-name
     :address (fn [values env k]
                (if (= arity (count values))
                  (resume k (apply var-value values))
                  (wrong "Incorrect arity"
                         (list var-name values))))}))
(defmethod invoke ::primitive [f values env k]
  ((:address f) values env k))

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
  {:type :done, :value v})

(defn eval* [expr]
  (eval-loop expr env-global {:type 'top-eval}))

(is (= 1  (eval* '((lambda (a b) a) 1 2))))
(is (= 'a (eval* '(if true (quote a) 2))))
(is (= 5  (eval* '(if f 1 5))))
(is (= 3  (eval* '(begin 1 2 3))))
(is (= 9  (eval* '((lambda (a) (set! a 9) a) 5))))
(is (= empty-begin (eval* '(begin))))

(is (= 7 (eval* '(+ 2 5))))
(is (= true (eval* '(< 2 5))))
(is (= the-false-value (eval* '(> 2 5))))

(is (= 9 (eval* '((lambda (a b) (+ a b)) 3 6))))

(is (= 120 (eval*
            '((lambda (fact n)
                      (fact fact n))
              (lambda (fact n)
                      (if (< n 2)
                        1N
                        (* n (fact fact (- n 1)))))
              50))))
