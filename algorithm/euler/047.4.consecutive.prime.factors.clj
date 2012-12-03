;; From problem 12
(defn prime-factors-of
  "Returns a sorted list of prime factors of num, including multiplicity."
  [num]
  (let [q (Math/sqrt num)
        factor? (fn [nom den] (zero? (rem nom den)))]
    (loop [n num
           d 2
           r []]
      (cond
       (> d q) (concat r [n])
       (= n d) (concat r [n])
       (factor? n d) (recur (/ n d) d (conj r d))
       true          (recur n (inc d) r)))))

(defn tweaked-factors [factors]
  (let [f (frequencies (prime-factors-of factors))]
    (map #(* % (f %)) (keys f))))

(defn euler-47 [n]
  (let [groups (partition n 1 (iterate inc 2))
        myfn (fn [g]
               (let [f (map #(tweaked-factors %) g)]
                 (and (every? #(= n (count %)) f)
                      (distinct? (apply concat f)))))]
    (first (filter myfn groups))))
