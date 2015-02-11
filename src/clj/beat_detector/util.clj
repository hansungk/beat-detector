(ns beat-detector.util)

(declare take-raw-recur)
(defn take-raw
  "Takes n data from chunked raw data and makes them into one sound data.
  Returns resulting data."
  [n raw]
  (take-raw-recur [] n raw))

(declare take-data conj-data drop-raw)
(defn take-raw-recur
  "Helper function for take-raw.
  Carries so-far-taken data and recurs until enoughly consumed."
  [taken n raw]
  (let [head (first raw)
        m (count (first head))]
    (cond ; Check if first chunk is enough
      (>= 0 n) []
      (>= 0 m) taken
      (>= m n) (let [new-taken (take-data n head)]
                 (conj-data taken new-taken))
      (< m n)  (recur (conj-data taken head) (- n m) (drop-raw m raw)))))

(declare drop-data)
(defn drop-raw
  "Drops n datas from chunked raw data.
  Removes whole chunk when a chunk gets depleted."
  [n raw]
  (let [head (first raw)
        m (count (first head))]
    (cond ; Check if first chunk is enough
          (or (>= 0 n) (>= 0 m)) raw
          (> m n) (let [new-head (drop-data n head)]
                    (conj (rest raw) new-head))
          (<= m n) (recur (- n m) (rest raw)))))

(defn take-data
  "Clojure's 'take' function implemented on sound data.
  Returns [(take n left-data) (take n right-data)]."
  [n data]
  (if (>= 0 n)
    []
    (mapv (fn [x] (vec (take n x))) data)))

(defn drop-data
  "Clojure's 'drop' function implemented on sound data.
  Returns [(drop n left-data) (drop n right-data)]."
  [n data]
  (if (<= (count (first data)) n)
    []
    (mapv (fn [x] (vec (drop n x))) data)))

(defn split-at-data
  "Clojure's 'split-at' function implemented on sound data.
  Returns [(take-data n data) (drop-data n data)]."
  [n data]
  [(take-data n data) (drop-data n data)])

(defn conj-data
  "Conjoins two sound data into one."
  [data1 data2]
  (let [left1 (vec (first data1))
        left2 (vec (first data2))
        right1 (vec (second data1))
        right2 (vec (second data2))]
    [(into left1 left2) (into right1 right2)]))

(defn average
  "Calculates average value from the given coll."
  [coll]
  (double (/ (apply + coll) (count coll))))

(defn variance-avg
  "Returns the variance of the energies from the given buffer
  (default 43-length)."
  [buffer]
  (let [avg (average buffer)]
    (average (map (fn [x] (* (- x avg) (- x avg))) buffer))))

(defn sumsq
  "Calculate sum square of two numbers."
  [x y]
  (+ (* x x) (* y y)))

(defn sound-energy
  "Returns the total sound energy of the 2-channel data.
  Sound energy is calculated by sum square of the amplitude of each
  channel."
  [data]
  (let [left (first data)
        right (second data)
        N (count left)]
    (* 100 (Math/sqrt (/ (apply + (map sumsq left right)) (* 2 N))))))

(defn peak-threshold-factor
  "Returns C, the factor threshold for an energy peak to be detected as
  a beat, which is determined by the variance of sound energy."
  [variance]
  (comment 1.2)
  (do (println (+ 1.5142857 (* -0.0025714 variance)))
      (+ 1.5142857 (* -0.0025714 variance))))

(defn determine-subbands-beat
  "Determines beat from given 1-D buffer using linear peak energy factor
  variation."
  [buffer]
  (let [C (peak-threshold-factor (variance-avg buffer))
        E (average buffer)]
    (> (peek buffer) (* C E))))

(comment (defn trim-adjacent
  "Removes adjacent numbers from given numbers vector. Allows one skip in
  adjacency checking. Leaves only the head of each adjacencies."
  [coll]
  (reduce (fn [xs y]
            (if (empty? xs)
              [y]
              (if (> (- y (last xs)) 5)
                (conj xs y)
                xs))) [] coll)))

(defn trim-adjacent
  "Removes adjacent numbers from given numbers vector. Allows one skip in
  adjacency checking. Leaves only the head of each adjacencies."
  [coll]
  (first
    (reduce (fn [[xs x1] x2] ; xs: trimmed coll so far
                             ; x1: last read value
                             ; x2: newly read value
              (if (<= (- x2 x1) 2)
                [xs x2] ; consecutive, no add
                [(conj xs x2) x2])) ; new head, add
            [[] -3] coll)))

(defn shift
  "Increases given numbers by d."
  [d nums]
  (vec (map (partial + d) nums)))

(defn find-near
  "Finds an element of nums that is the nearest to the given x."
  [^long x nums]
  (let [diffs (map (fn [^long y] (Math/abs (long (- y x)))) nums)
        idx (.indexOf diffs (apply min diffs))]
    (nums idx)))

(def ^:constant C 2)

(defn smooth
  "Smooth out numbers vector.
  If a magnitude of each number is smaller than 'C', zero it out."
  [nums]
  (map (fn [^long x] (if (<= (Math/abs x) C) 0 x)) nums))

(defn autocorrelate
  "Subtracts each value of nums to the nearest value in nums shifted by d."
  [nums d]
  (let [snums (shift d nums)]
    (smooth (map (fn [x] (- x (find-near x nums))) snums))))

(defn corr-zerocounts
  "Returns a vector of counts of zeros from autocorrelations of times.
  Each element at index matches counts of zeros occurred in autocorrelation by
  index."
  [times]
  (loop [d (inc C) hits (vec (repeat d 0))]
    (if (<= d 300)
      (recur (inc d) (conj hits (count (filter zero? (autocorrelate times d)))))
      hits)))

(defn estimated-interval
  "Returns estimated interval of instances by which given beats repeat by
  a specific pattern."
  [times]
  (let [zerocounts (corr-zerocounts times)
        maxcount (apply max zerocounts)]
    (+ (.indexOf zerocounts maxcount) (quot C 2))))
