(ns beat-detector.util)

(declare take-raw-recur)
(defn take-raw
  "Takes n data from chunked raw data and makes them into one sound data."
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
      (>= 0 n) [[] raw]
      (>= m n) (let [new-taken (take-data n head)]
                 [(conj-data taken new-taken) (rest raw)])
      (< m n)  (recur (conj-data taken head) (- n m) (drop-raw m raw)))))

(declare split-at-data)
(defn drop-raw
  "Drops n datas from chunked raw data.
  Removes whole chunk when a chunk gets depleted."
  [n raw]
  (let [[data new-head] (split-at-data n (first raw))
        taken (count (first data))]
    (if (empty? (first new-head))
      (if (< taken n)
        (recur (- n taken) (rest raw))
        (rest raw))
      (conj (rest raw) new-head))))

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
  (mapv (fn [x] (drop n x)) data))

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

(defn fill-data-buffer
  "Returns a vector of [buffer rest-raw], where buffer is the new data
  buffer newly filled with just enough (>=n) sound datas, and rest-raw
  is the new raw data with its old contents at the front dropped."
  [buffer raw n]
  (if (empty? raw) ; Check if reached end of song
    [buffer raw]
    (if (< (count (first buffer)) n) ; Check if buffer is already filled enough (fake call)
      (recur (map into buffer (first raw)) (rest raw) n)
      [buffer raw])))

(defn average-local-energy
  "Returns average energy from the given energy-buffer
  (default 43-length)."
  [energy-buffer])

(defn sound-energy
  "Returns the total sound energy of the 2-channel data.
  Sound energy is calculated by sum square of the amplitude of each
  channel."
  [data]
  (let [left (first data)
        right (second data)]
    (reduce + (map (fn [x y] (+ (* x x) (* y y))) left right))))
