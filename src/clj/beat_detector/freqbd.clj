(ns beat-detector.freqbd
  (:require [beat-detector.util :refer :all]
            [beat-detector.loader :only duration-inst :as loader]
            [beat-detector.packet :as packet])
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

(defn log->linear
  "Converts logarithmic 1~n-freq into linear 1~n-inst."
  [i n-inst n-freq]
  (Math/ceil (* (/ n-inst 44100.0) 20.0 (Math/pow 1000 (/ i n-freq)))))

(defn divide ; FIXME too slow (0.6ms)
  "Divides the n-inst-length fft buffer into n-freq-length fft buffer, summing
  each grouped magnitude values."
  [buffer n-inst n-freq]
  (loop [dest [] src buffer n 1]
    (if (<= n n-freq)
      (let [index-i (log->linear (dec n) n-inst n-freq)
            index-f (log->linear n n-inst n-freq)
            n-take (- index-f index-i)]
        (if (< n-take 1) ; Don't consume src
          (recur (conj dest (* (/ 1 n-inst) (first src))) src (inc n))
          (recur (conj dest (* (/ n-take n-inst) (apply + (take n-take src))))
                 (drop n-take src)
                 (inc n))))
      dest)))

(defn generate-fft-buffer
  "Computes FFT on the first n-inst samples of raw and returns
  n-inst-length fft magnitudes vector. Doesn't consume raw."
  [raw n-inst n-freq]
  (let [src (take-raw n-inst raw) ;FIXME too slow
        re (first src)
        im (second src)]
    (if (= (count re) n-inst) (divide (fft re im) n-inst n-freq)))) ; FIXME only accept 1024

(defn generate-energy-subbands-buffer
  "Generates new energy subbands buffer of length n-hist/n-inst,
  containing n-freq subbands, from raw. raw remains intact."
  [raw n-inst n-hist n-freq]
  (loop [buf (vec (repeat n-freq []))
         raw_ raw
         n (/ n-hist n-inst)]
    (if (> n 0)
      (let [fft-buffer (generate-fft-buffer raw_ n-inst n-freq)]
        (recur (map conj buf fft-buffer)
               (drop-raw n-inst raw_)
               (dec n)))
      buf)))

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
  (let [fft-buffer (generate-fft-buffer raw n-inst n-freq)
          pre (heads-off buffer)]
      (map conj pre fft-buffer)))

(defn- heads-off
  "Drops head of each subbands from energy subband buffer."
  [buffer]
  (mapv (fn [x] (vec (drop 1 x))) buffer))

(defn determine-beat
  "Given energy subbands buffer, executes beat determination algorithm to each
  subbands. A beat is detected in a subband when its energy is higher than the
  average energy of that subband times C (default: 250)."
  [{buffer :buffer}]
  (let [tails (map last buffer)
        avgs (map average buffer)
        variances (map variance-avg buffer)
        C 3.0
        V0 1.0] ; FIXME Implement replayGain?
                ; TODO Segment beats (verse, chorus,...) and process each
                ; independently
    (mapv #(every? true? (vector %1 %2))
          (map (fn [x y] (> x (* C y))) tails avgs) ; E > C*Eavg filtering
          (map #(> % V0) variances)))) ; V > V0 Filtering

(defn initialize
  "Factory function that returns an initialized Packet."
  [{raw :raw n-inst :n-inst n-hist :n-hist n-freq :n-freq :as packet}]
  (if (empty? raw) ; If raw is empty, return nil
    nil
    (let [new-buffer (generate-energy-subbands-buffer raw n-inst n-hist n-freq)
          rest-raw (drop-raw n-hist raw)]
      (assoc packet :buffer new-buffer :raw rest-raw :pos (/ n-hist n-inst)))))

(defn reload
  "Reloads buffer of packet with new energy subbands value of 1024 samples from
  raw, preparing the packet for the next processing step."
  [{raw :raw buffer :buffer pos :pos n-inst :n-inst n-hist :n-hist n-freq :n-freq :as packet}]
  (let [next-buffer (next-energy-subbands-buffer buffer raw n-inst n-freq)
        rest-raw (drop-raw n-inst raw)]
    (assoc packet :buffer next-buffer :raw rest-raw :pos (inc pos))))

(declare update-result)
(defn print-progress
  [pos]
  (print "Progress:"
         (/ (long (* 1000.0 (/ (inc pos) loader/duration-inst))) 10.0)
         "%\r"))

(defn process
  "Processes given initialized packet and returns detection result.
  result is a 2-D vector that consists of detected beat instance indices.  Each
  1-D vectors in the result match with n-freq frequency subbands in the same
  order."
  [{raw :raw pos :pos :as packet} result]
  (if (nil? (second raw)) ; If raw gets smaller than 1024, FFT becomes
                                  ; impossible - so simply disregard last chunk
    result
    (recur (reload packet) ; future packet
           (update-result result (determine-beat packet) ; FIXME time later
                          (do
                            (print-progress pos)
                            pos)))))

(defn- update-result
  "Conjoins result with 'binary' result vector returned from determine-beat."
  [result binary pos]
  (doall (map into result (replace {true [pos] false []} binary))))
  ; If no doall, LazySeq does not evaluate and cause stackoverflow error

(defn start
  "Starts frequency selected beat detection algorithm using given Packet
  data."
  [raw]
  (let [initial-packet (initialize (packet/pack raw))
        initial-result (vec (repeat (:n-freq initial-packet) []))]
    (map trim-adjacent (process initial-packet initial-result))))
