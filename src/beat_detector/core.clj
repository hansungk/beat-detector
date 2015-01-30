(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]))

(def ^:dynamic *sound* (dynne/read-sound "sample.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *sound-history-num* 44032)
(def ^:dynamic *instance-num* 1024)
(def ^:dynamic *energy-history-num* 43)
(def ^:dynamic *chunk-size* 10000)

(declare take-data drop-data conj-data split-at-data drop-raw)
(declare take-raw-recur)
(defn take-raw
  "Takes n data from chunked raw data and makes them into one sound data."
  [n raw]
  (take-raw-recur [] n raw))

(defn take-raw-recur
  "Helper function for take-raw.
  Carries so-far-taken data and recurs until enoughly consumed."
  [taken n raw]
  (let [head (first raw)
        m (count (first head))]
    (cond ; Check if first chunk is enough
      (>= m n) (let [new-taken (take-data n head)]
                 [(conj-data taken new-taken) (rest raw)])
      (< m n)  (recur (conj-data taken head) (- n m) (drop-raw m raw)))))

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

(defn take-data
  "Clojure's 'take' function implemented on sound data.
  Returns [(take n left-data) (take n right-data)]."
  [n data]
  (mapv (fn [x] (take n x)) data))

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

(defn- initial-energy-buffer
  "Constructs a new energy buffer from scratch and returns it."
  [data-buffer instance-num history-num]
  (loop [buf '()
         data-buf data-buffer]
    (if (< (count (first data-buf)) instance-num) ; Check if data-buffer is shorter than instance-num; that is all consumed
      [buf data-buf]
      (let [[data-cons data-new] (split-at-data instance-num data-buf)]
        (recur (conj buf (sound-energy data-cons)) data-new)))))

(defn energy-variance
  "Returns the variance of the energies from the given energy-buffer
  (default 43-length)."
  [energy-buffer])

(defn peak-threshold-factor
  "Returns the factor threshold for an energy peak to be detected as a
  beat, which is determined by the local variance energy-variance of
  sound energy."
  [energy-variance])

(defn determine-beat
  "Returns whether the instance is determined as a beat, which is held
  by examining the factor between the energy of local peak and that of
  local average."
  [instant-energy average-local-energy threshold-factor])

(defn start-detection
  [raw history-num instance-num])

(defn tester
  []
  (let [data-buffer (first (fill-data-buffer ['() '()] *raw-data* *instance-num*))]
    (second (initial-energy-buffer data-buffer *instance-num* *sound-history-num*))))
