(ns beat-detector.simplebd
  (:use [beat-detector.util]))

(declare sound-energy)
(defn next-energy-buffer
  "Refreshes energy buffer and returns it. buffer should not be empty.
  'Shift' data 1 index to the left and conj new energy value from raw at
  the right end (as per vector).  When there is no more remaining raw,
  it appends zeros, which would never be falsely detected as beats."
  [buffer raw n-inst]
  (if (empty? buffer)
    nil   ; FIXME is this necessary?
    (let [energy (sound-energy (take-raw n-inst raw))]
      (conj (vec (rest buffer)) energy))))

(defn gen-energy-buffer
  "Generates new energy buffer of length
  *energy-history-num*/*instance-num*, extracted from raw."
  [raw n-inst n-hist]
  (loop [buf [] raw' raw n (/ n-hist n-inst)]
    (if (> n 0)
      (let [energy (sound-energy (take-raw n-inst raw'))]
        (recur (conj buf energy) (drop-raw n-inst raw') (dec n)))
      buf)))

(defn sound-energy
  "Returns the total sound energy of the 2-channel data.
  Sound energy is calculated by sum square of the amplitude of each
  channel."
  [data]
  (let [left (first data)
        right (second data)]
    (reduce + (map (fn [x y] (+ (* x x) (* y y))) left right))))

(defn energy-variance
  "Returns the variance of the energies from the given buffer
  (default 43-length)."
  [buffer]
  (let [avg (average buffer)]
    (average (map (fn [x] (* (- x avg) (- x avg))) buffer))))

(defn peak-threshold-factor
  "Returns C, the factor threshold for an energy peak to be detected as
  a beat, which is determined by the variance of sound energy."
  [variance]
  (comment (+ 1.5142857 (* -0.0025714 variance)))
  1.3) ; FIXME the formula doesn't work well

(defn determine-beat
  "Given energy-buffer, determines wither the target instance is a beat,
  which is held by examining the factor between the energy of local peak
  and that of local average."
  [buffer]
  (let [C (peak-threshold-factor (energy-variance buffer))
        E (average buffer)]
    (> (peek buffer) (* C E))))

(defn prepare
  "Prepares the first Packet to trigger detection.
  Essentially generates the first ever energy buffer from raw."
  [packet]
  (let [{raw :raw n-inst :n-inst n-hist :n-hist} packet
        new-buffer (gen-energy-buffer raw n-inst n-hist)
        rest-raw (drop-raw n-hist raw)]
    (assoc packet :buffer new-buffer :raw rest-raw)))

(defn update
  "Updates Packet and prepares it for the next processing step.
  Essentially calls next-energy-buffer and consumes raw by one instance."
  [packet]
  (let [{buffer :buffer raw :raw n-inst :n-inst} packet
        next-buffer (next-energy-buffer buffer raw n-inst)
        rest-raw (drop-raw n-inst raw)]
    (assoc packet :buffer next-buffer :raw rest-raw)))

(defn trigger
  [raw n-inst n-hist]
  (gen-energy-buffer raw n-inst n-hist))
