(ns us.chouser.LISP.y)

(def fix
  (fn [f] ((fn [x] (x x))
           (fn [x] (f (fn [y] ((x x) y)))))))

(defn fix [maker]
  ((fn [x] (x x))
   (fn [x] (maker (fn [y] ((x x) y))))))

(defn make-fact [factorial]
  (fn real-fact [n]
    (if (= n 0) 1 (* n (factorial (- n 1))))))

(def factorialR
  (fix make-fact))

(factorialR 5)

;; ---

#_
(defn fix2 [maker]
  (dup
   (fn thing2 [t2]
     (maker (fn impl [a b]
              ((dup t2) a b))))))

(defn dup [f]
  (f f))

(defn fixN [maker]
  (dup
   (fn thing2 [t2]
     (maker (fn impl [& args]
              (apply (dup t2) args))))))

(defn make-fact2 [factorial]
  (fn impl-fact2 [n base]
    (if (= n 0) base (* n (factorial (- n 1) base)))))

(def factorialR2
  (fixN make-fact2))

(assert (= (factorialR2 5 2) 240))

;; ---

(defn dup2 [f1 f2]
  [(f1 f1 f2)
   (f2 f1 f2)])

(defn fix2N [maker1 maker2]
  (dup2
   (fn thing1 [t1 t2]
     (maker1 (fn impl1 [& args]
               (apply (first (dup2 t1 t2)) args))
             (fn impl2 [& args]
               (apply (second (dup2 t1 t2)) args))))
   (fn thing2 [t1 t2]
     (maker2 (fn impl1 [& args]
               (apply (first (dup2 t1 t2)) args))
             (fn impl2 [& args]
               (apply (second (dup2 t1 t2)) args))))))

(def odd-and-even
  (fix2N (fn odd-maker [o? e?]
           (fn [n]
             (if (= n 0)
               false
               (e? (- n 1)))))
         (fn even-maker [o? e?]
           (fn [n]
             (if (= n 0)
               true
               (o? (- n 1)))))))

((first odd-and-even) 5)

;; fixNN takes a map of makers and returns a map of impls

(defn dupN [f-map]
  (update-vals f-map (fn [f] (f f-map))))

#_
(defn fixNN [maker-map]
  (dupN
   (update-vals maker-map
                (fn [maker]
                  (fn thing [thing-map]
                    (maker (->> thing-map
                                (map (fn [[k thing]]
                                       [k (fn impl [& args]
                                            (apply (get (dupN thing-map) k) args))]))
                                (into {}))))))))

(defn fixNN [maker-map]
  (dupN
   (update-vals maker-map
                (fn [maker]
                  (fn thing [thing-map]
                    (let [*impl-map (delay (dupN thing-map))]
                      (maker (->> thing-map
                                  (map (fn [[k thing]]
                                         [k (fn impl [& args]
                                              (apply (get @*impl-map k) args))]))
                                  (into {})))))))))

(defn fixNN [maker-map]
  (dupN
   (update-vals maker-map
                (fn [maker]
                  (fn thing [thing-map]
                    (let [*impl-map (delay (dupN thing-map))]
                      (maker (->> thing-map
                                  (map (fn [[k thing]]
                                         [k (fn impl [& args]
                                              (apply (get @*impl-map k) args))]))
                                  (into {})))))))))

(def odd-and-even
  (fixNN {:odd? (fn [impl-map]
                  (fn [n]
                    (if (= n 0)
                      false
                      ((:even? impl-map) (- n 1)))))
          :even? (fn [impl-map]
                   (fn [n]
                     (if (= n 0)
                       true
                       ((:odd? impl-map) (- n 1)))))}))

(assert (= true ((:odd? odd-and-even) 5)))
(assert (= false ((:even? odd-and-even) 5)))


(def klop
  (let [r (fn [s c h e m]
            (fn [f]
              (f (fn [n]
                   (((m e c h e s) f) n)))))]
    (r r r r r r)))

(def kfact
  (klop (fn make-fact [factorial]
          (fn real-fact [n]
            (if (= n 0) 1 (* n (factorial (- n 1))))))))

(assert (= 120 (kfact 5)))
