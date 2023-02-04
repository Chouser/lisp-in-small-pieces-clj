(ns ezducerdemo
  (:require [clojure.core.reducers :as r]))

;;
;; 1st take
;;

(defn ezducer [constructor]
    (fn [reducer]
      (let [steps (fn [a vs]
                    ;; Use loop/recur instead of reduce to avoid reduce's
                    ;; swallowing the special 'reduced' values.
                    (loop [a a vs vs]
                      (if (or (reduced? a) (empty? vs))
                        a
                        (if (= (first vs) reduced)
                          (reduced a)
                          (recur (reducer a (first vs)) (rest vs))))))
            {:keys [step, result]
             :or {step (fn [v] [v])
                  result (fn [] [])}} (constructor)]
        (fn
          ([] (reducer))
          ([a v] (steps a (step v)))
          ([a] (reducer (unreduced (steps a (result)))))))))

(defn ezfilter [pred]
  (ezducer
    (fn []
      {:step (fn [v] (if (pred v) [v] []))})))

(def drop-all
  (ezducer
    (fn []
      {:step (fn [_v] [])})))

(defn eztake [n]
  (if (< n 1)
    drop-all
    (ezducer
      (fn []
        (let [*n* (atom n)]
          {:step (fn [v]
                   (if (< (swap! *n* dec) 1)
                     [v reduced]
                     [v]))})))))

(defn demo [msg xf] [msg (into [] xf (range 10))])
(prn [(demo "Keep odd numbers" (ezfilter odd?))
      (demo "take first 3" (eztake 3))
      (demo "Composing previous transducers" (comp (ezfilter odd?) (eztake 3)))])

(defn rcount
  ([] 0)
  ([n] n)
  ([n _] (inc n)))

(time (transduce (eztake 1000000) rcount (range 1500000))) ;; 220 msecs

;;
;; 2nd take
;;

(defn steps [reducer a vs]
  (if (or (reduced? a) (empty? vs))
    a
    (let [[v & vs] vs]
      (if (= v reduced)
        (reduced a)
        (recur reducer
               (reducer a v) ;; step
               vs)))))

(defn steps-reducer [reducer f]
  (fn
    ([] (reducer)) ;; init
    ([a v] (steps reducer a (f v))) ;; step
    ([a] (reducer (unreduced (steps reducer a (f))))))) ;; result

(defn ezducer2
  ([f] ;; stateless
   (fn [reducer]
     (steps-reducer reducer f)))

  ([f state] ;; stateful
   (fn [reducer]
     (let [*state* (atom state)
           stateful-f (fn
                       ([] (let [[_ vs] (f @*state*)] vs))
                       ([v] (let [[state vs] (f @*state* v)]
                              (reset! *state* state)
                              vs)))]
       (steps-reducer reducer stateful-f)))))

(defn ezfilter2 [pred]
  (ezducer2
    (fn
      ([] [])
      ([v] (if (pred v) [v] [])))))

(def drop-all2
  (ezducer2
    (fn
      ([] [])
      ([_] []))))

(defn eztake2 [n]
  (if (< n 1)
    drop-all2
    (ezducer2
      (fn
        ([n] [n []])
        ([n v]
         (let [n (dec n)]
           [n (if (< n 1) [v reduced] [v])])))
      n)))

(defn demo [msg xf] [msg (into [] xf (range 10))])
(prn [(demo "[2nd] Keep odd numbers" (ezfilter2 odd?))
      (demo "[2nd] take first 3" (eztake2 3))
      (demo "[2nd] Composing previous transducers" (comp (ezfilter2 odd?) (eztake2 3)))])

(time (transduce (eztake2 1000000) rcount (range 1500000))) ;; 180 msecs

;;
;; 3rd take
;;

(defn ezducer3 [& {:keys [step result state]}]
  (let [[step result]
        (if (nil? state)
          (let [step' (if (nil? step) (fn [v] [v]) step)
                result' (if (nil? result) (fn [] []) result)]
            [step' result'])
          (let [*state* (atom state)

                step' (if (nil? step) (fn [s v] [s [v]]) step)
                result' (if (nil? result) (fn [s] [s []]) result)

                step'' (fn [v]
                        (let [[state vs] (step' @*state* v)]
                          (reset! *state* state)
                          vs))
                result'' (fn []
                          (let [[_state vs] (result' @*state*)]
                            vs))]
            [step'' result'']))]
    (fn [reducer]
      (letfn [(steps [a [v :as vs]]
                (if (or (reduced? a) (empty? vs))
                  a
                  (if (= v reduced)
                    (reduced a)
                    (recur (reducer a v) ;; step
                           (rest vs)))))]
        (fn
          ([] (reducer)) ;; init
          ([a v] (steps a (step v)))
          ([a] (reducer (unreduced (steps a (result)))))))))) ;; result

(defn ezfilter3 [pred]
  (ezducer3
    :step (fn [v] (if (pred v) [v] []))))

(def drop-all3
  (ezducer3
    :step (fn [_] [])))

(defn eztake3 [n]
  (if (< n 1)
    drop-all3
    (ezducer3
      :state n
      :step (fn [n v]
              (let [n (dec n)]
                [n (if (< n 1)
                     [v reduced]
                     [v])])))))

(prn [(demo "[3rd] Keep odd numbers" (ezfilter3 odd?))
      (demo "[3rd] take first 3" (eztake3 3))
      (demo "[3rd] Composing previous transducers" (comp (ezfilter3 odd?) (eztake2 3)))])

(time (transduce (eztake3 1000000) rcount (range 1500000))) ;; 325 msecs

;;
;; 4th take
;;

(defn ezducer4
  ([constructor]
   (fn [reducer]
     (let [*a* (atom :wat) ;; TODO: unnecessary?
           step! (fn
                   ([] (reduced? (swap! *a* (fn [a] (if (reduced? a) a (reduced a))))))
                   ([v] (reduced? (swap! *a* (fn [a v] (if (reduced? a) a (reducer a v))) v))))
           {:keys [step result]} (constructor step!)
           step (if (nil? step) (fn [v] (step! v)) step)
           result (if (nil? result) (fn []) result)]
       (fn
         ([] (reducer)) ;; init
         ([a v] (reset! *a* a) (step v) @*a*)
         ([a] (reset! *a* a) (result) (reducer (unreduced @*a*))))))) ;; result
  ([state constructor]
   (fn [reducer]
     (let [*a* (atom state)
           step! (fn
                   ([] (reduced? (swap! *a* (fn [a] (if (reduced? a) a (reduced a))))))
                   ([v] (reduced? (swap! *a* (fn [a v] (if (reduced? a) a (reducer a v))) v))))
           {:keys [step result]} (constructor step!)
           step (if (nil? step) (fn [state v] (step! v) state) step)
           result (if (nil? result) (fn [_]) result)
           *state* (atom state)]
       (fn
         ([] (reducer)) ;; init
         ([a v] (reset! *a* a) (swap! *state* step v) @*a*)
         ([a] (reset! *a* a) (swap! *state* result) (reducer (unreduced @*a*)))))))) ;; result

(def drop-all4
  (ezducer4
    (fn [step]
      {:step (fn [_])})))

(defn eztake4 [n]
  (if (< n 1)
    drop-all4
    (ezducer4
      n ;; state initialized
      (fn [step]
        {:step (fn [n v]
                 (step v)
                 (let [n (dec n)]
                   (when (< n 1)
                     (step))
                   n))}))))

(prn [(demo "[4th] Keep odd numbers" (ezfilter4 odd?))
      (demo "[4th] take first 3" (eztake4 3))
      (demo "[4th] Composing previous transducers" (comp (ezfilter3 odd?) (eztake4 4)))])

(time (transduce (eztake4 1000000) rcount (range 1500000))) ;; 55 msecs

;;
;; take4c: Chouser's riff on take4
;;

(defn ezducer4c [constructor]
  (fn transducer [reducer]
    (let [*a (volatile! :overwrite-me)
          step2 (fn step2 [v]
                  (when-not (reduced? @*a)
                    (vswap! *a reducer v))
                  :ignorable)
          done (fn done []
                 (when-not (reduced? @*a)
                   (vswap! *a reduced))
                 :ignorable)

          {:keys [step1 result]} (constructor {:step2 step2, :done done})
          step1 (or step1 step2)
          result (or result (fn default-result []))]
      (fn wrapped-reducer
        ([] (reducer)) ;; init
        ([a v] (vreset! *a a) (step1 v) @*a)
        ([a] (vreset! *a a) (result) (reducer (unreduced @*a))))))) ;; result

(def drop-all4c
  (ezducer4c
   (fn [_]
      {:step1 (fn [_])})))

(defn ezfilter4c [pred]
  (ezducer4c
   (fn [{:keys [step2]}]
     {:step1 (fn [v] (when (pred v) (step2 v)))})))

(defn eztake4c [n]
  (if (< n 1)
    drop-all4c
    (let [*n (volatile! n)]
      (ezducer4c
       (fn [{:keys [step2 done]}]
         {:step1 (fn [v]
                  (step2 v)
                  (when (< (vswap! *n dec) 1)
                    (done)))})))))

(prn [(demo "[4c] Keep odd numbers" (ezfilter4c odd?))
      (demo "[4c] take first 3" (eztake4c 3))
      (demo "[4c] Composing previous transducers" (comp (ezfilter4c odd?) (eztake4c 4)))])


(defmacro ezducer4m [ctor-args & fns]
  `(ezducer4c
    (fn [{:keys ~ctor-args}]
      ~(->> (for [[_fn fn-name :as fn-body] fns]
              (do
                (assert (= _fn 'fn))
                (assert (symbol? fn-name))
                [(keyword fn-name)
                 fn-body]))
            (into {})))))

(def drop-all4m
  (ezducer4m []
    (fn step1 [_])))

(defn ezfilter4m [pred]
  (ezducer4m [step2]
    (fn step1 [v] (when (pred v) (step2 v)))))

(defn eztake4m [n]
  (if (< n 1)
    drop-all4m
    (let [*n (volatile! n)]
      (ezducer4m [step2 done]
        (fn step1 [v]
          (step2 v)
          (when (< (vswap! *n dec) 1)
            (done)))))))

(comment

  (time (transduce (eztake    1000000) rcount (range  1500000))) ;; 220 msecs
  (time (transduce (eztake4   1000000) rcount (range  1500000))) ;;  95 msecs

  (time (transduce (eztake4  10000000) rcount (range 15000000))) ;; 855 msecs
  (time (transduce (eztake4m 10000000) rcount (range 15000000))) ;; 320
  (time (transduce (take     10000000) rcount (range 15000000)))) ;; 225


#_
(defn filter-with-context [pred n coll]
  (let [step (fn step [buffer coll]
               (lazy-seq
                (when-let [[a & more] (seq coll)]
                  (if (pred (first coll))
                    (concat (take-last n buffer) [a] (step [] more))
                    (step (conj buffer a) more)))))]
    (step [] coll)))

#_
(filter-with-context #(or (= 4 %) (= 7 %)) 1 (range 20))


(defn filter-with-context [pred n coll]
  (let [padded (concat (repeat n :skipme)
                       coll
                       (repeat n :skipme))]
    (->> padded
         (partition (+ 1 (* 2 n)) 1)
         (filter #(pred (nth % n))))))

(filter-with-context #(or (= 4 %) (= 6 %)) 1 (range 7))
;; => ((3 4 5) (5 6 :skipme)) ;; current output

;; goal output:
;; => [{:match-index 4 :context [3 4 5]}
;;     {:match-index 5 :context [4 5 6]}]
;;
;; => [{:match-indexes [4 6] :context [3 4 5 6]}] ;; stretch goal

(require '[clojure.core.reducers :as r])

(defn filter-with-context [pred n coll]
  (into []
        (comp
         (partition (+ 1 (* 2 n)) 1)
         (filter #(pred (nth % n))))
        (concat (repeat n :skipme)
                coll
                (repeat n :skipme))))

(filter-with-context #(or (= 4 %) (= 6 %)) 1 (range 7))

;;;; Working examples:

;; primitive lazy-seq
(defn filter-with-context-ls [pred n coll]
  (let [step (fn step [buffer suffix coll]
               (lazy-seq
                (when-let [[v & more] (seq coll)]
                  (if (pred v)
                    (concat (take-last n buffer) [v] (step [] n more))
                    (if (pos? suffix)
                      (cons v (step [] (dec suffix) more))
                      (step (conj buffer v) 0 more))))))]
    (step [] 0 coll)))

(filter-with-context-ls #(or (= 1 %) (= 4 %) (= 11 %)) 2 (range 20))

;; loop/recur
(defn filter-with-context-lr [pred n coll]
  (loop [out [], buffer [], suffix 0, coll coll]
    (if-let [[v & more] (seq coll)]
      (if (pred v)
        (recur (-> out
                   (into (take-last n buffer))
                   (conj v))
               [] n more)
        (if (pos? suffix)
          (recur (conj out v) [] (dec suffix) more)
          (recur out (conj buffer v) 0 more)))
      out)))

(filter-with-context-lr #(or (= 1 %) (= 4 %) (= 11 %)) 2 (range 20))

;; ezducer (one of the variants)
(defn filter-with-context-ez [pred n]
  (let [*buffer (volatile! [])
        *suffix (volatile! 0)]
    (ezducer4c
     (fn [{:keys [step2]}]
       {:step1 (fn [v]
                 (if (pred v)
                   (do
                     (run! step2 (take-last n @*buffer))
                     (step2 v)
                     (vreset! *buffer [])
                     (vreset! *suffix n))
                   (if (pos? @*suffix)
                     (do
                       (step2 v)
                       (vswap! *suffix dec))
                     (vswap! *buffer conj v))))}))))

(into [] (filter-with-context-ez #(or (= 1 %) (= 4 %) (= 11 %)) 2)
      (range 20))

;; standard transducer
(defn filter-with-context-st [pred n]
  (fn transducer [r]
    (let [*buffer (volatile! [])
          *suffix (volatile! 0)]
      (fn
        ([] (r))
        ([a v]
         (if (pred v)
           (loop [a a
                  buf (take-last n @*buffer)]
             (if (seq buf)
               (let [a (r a (first buf))]
                 (if (reduced? a)
                   a
                   (recur a (rest buf))))
               (do
                 (vreset! *buffer [])
                 (vreset! *suffix n)
                 (r a v))))
           (if (pos? @*suffix)
             (do
               (vswap! *suffix dec)
               (r a v))
             (do
               (vswap! *buffer conj v)
               a))))
        ([a] (r a))))))

(into [] (filter-with-context-st #(or (= 1 %) (= 4 %) (= 11 %)) 2)
      (range 20))

;; high-order lazy seq
(defn filter-with-context-hl [pred n coll]
  (let [skipme (reify)
        padded (concat (repeat n skipme)
                       coll
                       (repeat n skipme))
        matches (->> padded
                     (partition (+ 1 (* 2 n)) 1)
                     (map #(zipmap [:idx :match :window] %&)
                          (range)
                          coll)
                     (filter #(pred (:match %))))]
    (->> matches
         (mapcat (fn [prev-m m]
                   (if-not prev-m
                     (:window m)
                     (drop (+ (- (:idx m))
                              (:idx prev-m)
                              (* 2 n)
                              1)
                           (:window m))))
                 (cons nil matches))
         (remove #(= % skipme)))))

(filter-with-context-hl #(or (= 1 %) (= 4 %) (= 11 %)) 2 (range 20))
