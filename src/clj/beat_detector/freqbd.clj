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
  "Computes FFT on the first n-inst samples of raw and returns 1024-length fft
  magnitudes vector."
  [raw n-inst]
  (fft (take-raw raw n-inst)))

(defn gen-energy-subbands-buffer
  [raw n-inst n-hist n-subfreq]
  (loop [buf (vec (take n-subfreq (repeat []))) raw' raw n (/ n-inst n-hist)]
    (if (> n 0)
      (let [fft-buffer (peek-fft-buffer raw n-inst)]
        )))) ;TODO

(defn initialize
  "Factory function that returns an initialized Packet."
  [packet]
  (let [{raw :raw n-inst :n-inst n-hist :n-hist} packet
        fft-buffer (peek-fft-buffer packet)
        rest-raw (drop-raw n-hist raw)]
    )) ;TODO

(defn start
  "Starts frequency selected beat detection algorithm using given Packet
  data."
  [packet]
  ) ;TODO
