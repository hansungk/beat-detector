(ns beat-detector.freqbd
  (:use [beat-detector.util])
  (:import [beat_detector FFT]))

(defn fft
  "Computes FFT on the coll and returns same-length fft magnitudes
  vector."
  [coll]
  (let [fft (FFT. (count coll))
        re (double-array coll)
        im (double-array (count coll))]
    (do
      (.fft fft re im)
      (map sumsq re im))))

(defn peek-fft-buffer
  "Computes FFT on the first n-inst samples of raw and returns
  1024-length fft magnitudes vector. Doesn't consume raw."
  [raw n-inst]
  (fft (take-raw raw n-inst)))

(declare heads-off)
(defn next-energy-subbands-buffer
  "Refreshes energy subbands buffer and returns it. buffer should not be
  empty.
  It will shift the subbands 1 index to the left and conj new subband
  energy value from fft at the right end (as per vector). When there is
  no more remaining raw, it appends zeros, which would never be falsely
  detected as beats."
  [buffer raw n-inst]
  (if (empty? buffer)
    nil
    (let [fft-buffer (peek-fft-buffer raw n-inst)
          pre (heads-off buffer)]
      (map conj pre fft-buffer))))

(defn- heads-off
  "Drops head of each subbands from energy subband buffer."
  [buffer]
  (vec (map (fn [x] (vec (drop 1 x))) buffer)))

(defn gen-energy-subbands-buffer
  "Generates new energy subbands buffer of length n-hist/n-inst,
  containing n-freq subbands, from raw. raw remains intact."
  [raw n-inst n-hist n-freq]
  (loop [buf (vec (take n-freq (repeat []))) raw' raw n (/ n-inst n-hist)]
    (if (> n 0)
      (let [fft-buffer (peek-fft-buffer raw' n-inst)]
        (recur (map conj buf peek-fft-buffer)
               (drop-raw n-inst raw')
               (dec n)))
      buf)))

(defn initialize
  "Factory function that returns an initialized Packet."
  [packet]
  (let [{raw :raw n-inst :n-inst n-hist :n-hist n-freq :n-freq} packet
        new-buffer (gen-energy-subbands-buffer raw n-inst n-hist n-freq)
        rest-raw (drop-raw n-hist raw)]
    (assoc packet :buffer new-buffer :raw rest-raw :pos (/ n-hist n-inst))))

(defn start
  "Starts frequency selected beat detection algorithm using given Packet
  data."
  [packet]
  ) ;TODO
