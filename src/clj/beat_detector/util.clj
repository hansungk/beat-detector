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
        right (second data)]
    (apply + (map sumsq left right))))

(defn peek-energy-buffer
  "Generates new energy buffer of length n-hist/n-inst from raw. raw
  remains intact."
  [raw n-inst n-hist]
  (loop [buf [] raw' raw n (/ n-hist n-inst)]
    (if (> n 0)
      (let [energy (sound-energy (take-raw n-inst raw'))]
        (recur (conj buf energy)
               (drop-raw n-inst raw')
               (dec n)))
      buf)))
