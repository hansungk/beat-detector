(ns beat-detector.freqbd
  (:use [beat-detector.util])
  (:import [beat_detector FFT]))

(def FFTc (FFT. 1024))
(defn fft
  "Computes FFT on the re/im coll and returns same-length fft magnitudes
  vector."
  [re im]
  (let [re-r (double-array re)
        im-r (double-array im)]
    (do
      (.fft FFTc re-r im-r)
      (map sumsq re-r im-r))))

(declare divide)
(defn peek-fft-buffer
  "Computes FFT on the first n-inst samples of raw and returns
  n-hist-length fft magnitudes vector. Doesn't consume raw."
  [raw n-inst n-freq]
  (let [src (take-raw n-inst raw) ;FIXME too slow
        re (first src)
        im (second src)]
    (divide (fft re im) n-freq)))

(defn divide ; FIXME too slow (0.6ms)
  "Divides the n-inst-length fft buffer into n-freq-length fft buffer, summing
  each grouped magnitude values."
  [buffer n-freq]
  {:pre [(zero? (rem (count buffer) n-freq))]}
  (vec (map (partial apply +) (partition (/ (count buffer) n-freq) buffer))))

(declare heads-off)
(defn next-energy-subbands-buffer
  "Refreshes energy subbands buffer and returns it. buffer should not be
  empty.
  It will shift the subbands 1 index to the left and conj new subband
  energy value from fft at the right end (as per vector). When there is
  no more remaining raw, it appends zeros, which would never be falsely
  detected as beats."
  [buffer raw n-inst n-freq]
  ;{:pre (not (empty? buffer))}
  (let [fft-buffer (peek-fft-buffer raw n-inst n-freq)
          pre (heads-off buffer)]
      (map conj pre fft-buffer)))

(defn- heads-off
  "Drops head of each subbands from energy subband buffer."
  [buffer]
  (vec (map (fn [x] (vec (drop 1 x))) buffer)))

(defn gen-energy-subbands-buffer
  "Generates new energy subbands buffer of length n-hist/n-inst,
  containing n-freq subbands, from raw. raw remains intact."
  [raw n-inst n-hist n-freq]
  (loop [buf (vec (repeat n-freq []))
         raw_ raw
         n (/ n-hist n-inst)]
    (if (> n 0)
      (let [fft-buffer (peek-fft-buffer raw_ n-inst n-freq)]
        (recur (map conj buf fft-buffer)
               (drop-raw n-inst raw_)
               (dec n)))
      buf)))

(defn initialize
  "Factory function that returns an initialized Packet."
  [packet]
  (if (empty? (:raw packet)) ; If raw is empty, return nil
    nil
    (let [{raw :raw n-inst :n-inst n-hist :n-hist n-freq :n-freq} packet
          new-buffer (gen-energy-subbands-buffer raw n-inst n-hist n-freq)
          rest-raw (drop-raw n-hist raw)]
      (assoc packet :buffer new-buffer :raw rest-raw :pos (/ n-hist n-inst)))))

(defn start
  "Starts frequency selected beat detection algorithm using given Packet
  data."
  [packet]
  ) ;TODO
