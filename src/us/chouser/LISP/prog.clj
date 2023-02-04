(ns us.chouser.LISP.prog)

(defn parse-prog-body [body]
  (reduce (fn [m form]
            (if (symbol? form)
              (assoc-in m
                        [:labels form]
                        (count (:instructions m)))
              (update m :instructions conj form)))
          {:instructions []
           :labels {}}
          body))

(defn go [index]
  (throw (ex-info "prog go" {:prog/ctrl {:go index}})))

(defn return [value]
  (throw (ex-info "prog go" {:prog/ctrl {:return value}})))

(defn prog-run [instructions]
  (loop [i 0]
    (let [ctrl (try ((nth instructions i))
                    (catch clojure.lang.ExceptionInfo ex
                      (or (:prog/ctrl (ex-data ex))
                          (throw ex))))]
      (or (:return ctrl)
          (recur (or (:go ctrl)
                     (inc i)))))))

(defmacro prog [locals & body]
  (let [{:keys [instructions labels]} (parse-prog-body body)]
    `(let [~@(mapcat list locals (repeat `(atom nil)))
           ~@(apply concat labels)]
       (prog-run ~(mapv (fn [instruction]
                          `(fn [] ~instruction))
                        instructions)))))

;;---

(defn fact [n]
  (let [n (atom n)]
    (prog [r]
          (reset! r 1)
     loop (reset! r (* (cond
                         (= @n 1) (return @r)
                         :else @n)
                       @r))
          (reset! n (- @n 1))
          (go loop))))

(assert (= 120 (fact 5)))


;; But it's so ugly to prefix every use a local with @, just because its
;; mutable (*wink*). So we could deep walk all the instruction forms, but this
;; would require we understand all Clojure's scoping rules. Relying instead on
;; Clojure's own symbol resolution, a bare symbol can only resolve to a local
;; (which is immutable), or to a Var which may be deref'ed on every use,
;; allowing it to operation as mutable.  Vars are only resolved using the
;; current namespace, which is globally visible, so we either accept that
;; awkward polution, or evaluate all the instructions in some other (temporary?)
;; namespace.  Let try the former, polluting solution first and see if it goes
;; anywhere at all.

(defmacro let! [bindings & body]
  `(do
     ~@(map (fn [sym] `(def ~(vary-meta sym assoc :dynamic true)))
            (take-nth 2 bindings))
     (binding ~bindings
       ~@body)))

(defmacro prog [locals & body]
  (let [{:keys [instructions labels]} (parse-prog-body body)]
    `(let! [~@(mapcat list locals (repeat nil))]
       (let [~@(apply concat labels)]
         (prog-run ~(mapv (fn [instruction]
                            `(fn [] ~instruction))
                          instructions))))))

;;---

(defn fact [uh-oh]
  (let! [n uh-oh]
    (prog [r]
          (set! r 1)
     loop (set! r (* (cond
                       (= n 1) (return r)
                       :else n)
                     r))
          (set! n (- n 1))
          (go loop))))

(assert (= 120 (fact 5)))

;; Ugh, what a terrible hack. Outer locals (like uh-oh) will shadow global vars
;; (like n) even though the `def` happens inside the scope of the local! Which
;; would cause `binding` to fail.  ...not to mention our `let!` bound names are
;; now left in the global namespace scope:

(assert (var? #'n))

;; Doing this nastiness in a separate macro-controlled namespace would only help
;; the pollution (at what performance cost?), but not do anything for the
;; var-shadowing.
