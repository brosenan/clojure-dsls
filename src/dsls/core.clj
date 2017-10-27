(ns dsls.core)

(defmacro defop [name args & body]
  (let [match (first args)]
    `(defn ~name ~args
       (cond (vector? ~match)
             (let [op# (first ~match)
                   opargs# (rest ~match)
                   f# (get op# ~name)]
               (cond (nil? f#)
                     (cond (-> op# meta :defterm)
                           (~name (apply op# opargs#) ~@(rest args))
                           :else (do ~@body))
                     :else
                     (f# ~@args)))
             :else (do ~@body)))))

(defop eval [x]
  (cond (vector? x)
        (let [[f & args] x
              args (map eval args)]
          (apply f args))
        :else x))

(def if {eval (fn [[_ cond then else]]
                (if (eval cond)
                  (eval then)
                  (eval else)))})

(defmacro defterm [name args def]
  `(def ~name ^:defterm (fn [~@args]
                          ~def)))

(defterm fact [n] [if [<= n 1]
                   1
                   [* n [fact [dec n]]]])


(defop in [s x]
  (cond (set? s) (contains? s x)
        (fn? s) (s x)
        :else (throw (Exception. (str "Cannot check membership for set " (pr-str s))))))

(defop enum [s]
  (cond (set? s)
        (seq s)
        :else
        (throw (Exception. (str "Cannot enumerate set " (pr-str s))))))

(defn associative [zero ops]
  (->>
   (for [[op func] ops]
     [op (fn [[concept & concept-args] & args]
           (cond (= (count concept-args) 0) (apply op zero args)
                 (= (count concept-args) 1) (apply op (first concept-args) args)
                 :else (let [concept-args (cond (> (count concept-args) 2)
                                                [(first concept-args) (vec (cons concept (rest concept-args)))]
                                                :else concept-args)]
                         (apply func (vec (cons concept concept-args)) args))))])
   (into {})))

(def universe
  {in (constantly true)})

(def intersect
  (associative [universe]
   {in (fn [[_ a b] x]
         (and (in a x)
              (in b x)))
    enum (fn [[_ a b]]
           (->> (enum a)
                (filter (partial in b))))}))


