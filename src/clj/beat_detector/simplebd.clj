(ns beat-detector.simplebd
  (:use [beat-detector.util]))

(defn next-energy-buffer
  "Refreshes energy buffer and returns it. buffer should not be empty.
  It will shift data 1 index to the left and conj new energy value from
  raw at the right end (as per vector). When there is no more remaining
  raw, it appends zeros, which would never be falsely detected as
  beats."
  [buffer raw n-inst]
  (if (empty? buffer)
    nil   ; FIXME is this necessary?
    (let [energy (sound-energy (take-raw n-inst raw))]
      (conj (vec (rest buffer)) energy))))

(defn peak-threshold-factor
  "Returns C, the factor threshold for an energy peak to be detected as
  a beat, which is determined by the variance of sound energy."
  [variance]
  (comment (+ 1.5142857 (* -0.0025714 variance))) ; FIXME this doesn't work well
  1.2
  )

(defn determine-beat
  "Given energy-buffer, determines wither the target instance is a beat,
  which is held by examining the factor between the energy of local peak
  and that of local average."
  [packet]
  (let [buffer (:buffer packet)
        C (peak-threshold-factor (variance-avg buffer))
        E (average buffer)]
    (> (peek buffer) (* C E))))

(defn initialize
  "Factory function that returns an initialized Packet."
  [packet]
  (if (empty? (:raw packet))
    nil
    (let [{raw :raw n-inst :n-inst n-hist :n-hist} packet
          new-buffer (peek-energy-buffer raw n-inst n-hist)
          rest-raw (drop-raw n-hist raw)]
    (assoc packet :buffer new-buffer :raw rest-raw :pos (/ n-hist n-inst)))))

(defn reload
  "Reloads buffer of packet with new energy value of 1024 samples from
  raw, preparing the packet for the next processing step."
  [packet]
  (let [{buffer :buffer raw :raw n-inst :n-inst pos :pos} packet
        next-buffer (next-energy-buffer buffer raw n-inst)
        rest-raw (drop-raw n-inst raw)]
    (assoc packet :buffer next-buffer :raw rest-raw :pos (inc pos))))

(defn process
  "Processes given initialized packet and returns detection result.
  result is a vector that consists of indices of beat-detected
  instances."
  [packet result]
  (if (empty? (:raw packet)) ; If raw is depleted, stop processing
    result
    (if (determine-beat packet)
      (recur (reload packet) (conj result (:pos packet)))
      (recur (reload packet) result))))

(defn trim-adjacent
  "Removes adjacent numbers from given numbers vector.
  Only leaves the head of each adjacencies."
  [coll]
  (reduce (fn [xs y]
            (if (empty? xs)
              [y]
              (if (> (- y (last xs)) 5)
                (conj xs y)
                xs))) [] coll))

(defn start
  "Starts simple beat detection algorithm using given Packet data."
  [packet]
  (trim-adjacent (process (initialize packet) [])))
