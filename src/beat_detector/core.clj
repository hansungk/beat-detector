(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]))

(def *sound* (dynne/read-sound "sample.wav"))
(def *raw-data* (dynne/chunks *sound* 44100)) 
;(def *chunk-count* (count *raw-data*))
(def *sound-history-num* 44032)
(def *instance-num* 1024)
(def *energy-history-num* 43)

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

(defn fill-data-buffer-pos
  "A version of fill-data-buffer that uses position value in the song,
  rather than passing raw data.
  Conclusion: little-to-no performance difference."
  [buffer pos n]
  (if (>= pos *chunk-count*) ; Check if reached end of song; pos starts from 0
    [buffer pos]
    (if (< (count (first buffer)) n) ; Check if buffer is already filled enough (fake call)
      (recur (map into buffer (nth *raw-data* pos)) (inc pos) n)
      [buffer pos])))

;(defn next-energy-buffer
;  "Returns a sound energy histroy buffer." ; TODO
;  [old-buffer data-buffer instance-num history-num]
;  (if (empty? old-buffer) ; Needs to be constructed from scratch
;    (recur (conj old-buffer energy) (drop-data-buffer intance-num data-buffer) instance-num history-num)
;    (if (= (count old-buffer) instance-num) ; Check if old-buffer is now an 'whole' buffer
;      )))

(defn- initial-energy-buffer
  "Constructs a new energy buffer from scratch and returns it."
  [data-buffer instance-num history-num]
  (loop [buf '()
         data-buf data-buffer]
    (if (< (count (first data-buf)) instance-num) ; Check if data-buffer is shorter than instance-num; that is all consumed
      [buf data-buf]
      (let [[data-cons data-new] (split-at-data-buffer instance-num data-buf)]
        (recur (conj buf (sound-energy data-cons)) data-new)))))

(defn take-data-buffer
  "Clojure's 'take' function implemented on data buffer.
  Returns [(take n left-data) (take n right-data)]."
  [n data-buffer]
  (mapv (fn [x] (take n x)) data-buffer))

(defn drop-data-buffer
  "Clojure's 'drop' function implemented on data buffer.
  Returns [(drop n left-data) (drop n right-data)]."
  [n data-buffer]
  (mapv (fn [x] (drop n x)) data-buffer))

(defn split-at-data-buffer
  "Clojure's 'split-at' function implemented on data buffer.
  Returns [(take-data-buffer n data-buffer) (drop-data-buffer n data-buffer)]."
  [n data-buffer]
  [(take-data-buffer n data-buffer) (drop-data-buffer n data-buffer)])

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
