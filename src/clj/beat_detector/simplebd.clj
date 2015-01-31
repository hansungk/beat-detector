(ns beat-detector.simplebd
  (:use [beat-detector.util]))

(declare sound-energy)
(defn next-energy-buffer
  "Refreshes energy buffer and returns it.
  If buffer is populated, 'shift' data 1 index to the left and conj
  new energy value from raw at the right end (as per vector).
  If buffer is empty, populate it with
  *energy-history-num*/*instance-num* new energy value from raw."
  [buffer raw n-inst]
  (let [energy (sound-energy (take-raw n-inst raw))]
    (conj (vec (rest buffer)) energy)))

(defn gen-energy-buffer
  "Generates new energy buffer of length
  *energy-history-num*/*instance-num*, extracted from raw."
  [raw n-inst n-hist]
  (loop [buf [] raw' raw n (/ n-hist n-inst)]
    (if (> n 0)
      (let [energy (sound-energy (take-raw n-inst raw'))]
        (recur (conj buf energy) (drop-raw n-inst raw') (dec n)))
      buf)))

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
