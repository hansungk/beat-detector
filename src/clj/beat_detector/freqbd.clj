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

(defn log->linear
  "Converts logarithmic 1~n-freq into linear 1~n-inst."
  [i n-inst n-freq]
  (Math/ceil (* (/ n-inst 44100.0) 20.0 (Math/pow 1000 (/ i n-freq)))))

(defn divide ; FIXME too slow (0.6ms)
  "Divides the n-inst-length fft buffer into n-freq-length fft buffer, summing
  each grouped magnitude values."
  [buffer n-inst n-freq]
  (comment (vec (map (fn [x] (* (/ n-freq 1024.0) (apply + x))) (partition (/ (count buffer) n-freq) buffer))))
  (loop [dest [] src buffer n 1]
    (if (<= n n-freq)
      (let [index-i (log->linear (dec n) n-inst n-freq)
            index-f (log->linear n n-inst n-freq)
            n-take (- index-f index-i)]
        (if (< n-take 1) ; Don't consume src
          (recur (conj dest (first src)) src (inc n))
          (recur (conj dest (apply + (take n-take src))) (drop n-take src) (inc n))))
      dest)))

(defn generate-fft-buffer
  "Computes FFT on the first n-inst samples of raw and returns
  n-inst-length fft magnitudes vector. Doesn't consume raw."
  [raw n-inst n-freq]
  (let [src (take-raw n-inst raw) ;FIXME too slow
        re (first src)
        im (second src)]
    (divide (fft re im) n-inst n-freq)))

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
  (vec (map (fn [x] (vec (drop 1 x))) buffer)))

(defn determine-beat
  "Given energy subbands buffer, executes beat determination algorithm to each
  subbands. A beat is detected in a subband when its energy is higher than the
  average energy of that subband times C (default: 250)."
  [packet]
  (let [buffer (:buffer packet)
        tails (map last buffer)
        avgs (map average buffer)
        C 3] ; 2 ~ 3
    (map (fn [x y] (> x (* C y))) tails avgs)))

(defn initialize
  "Factory function that returns an initialized Packet."
  [packet]
  (if (empty? (:raw packet)) ; If raw is empty, return nil
    nil
    (let [{raw :raw n-inst :n-inst n-hist :n-hist n-freq :n-freq} packet
          new-buffer (generate-energy-subbands-buffer raw n-inst n-hist n-freq)
          rest-raw (drop-raw n-hist raw)]
      (assoc packet :buffer new-buffer :raw rest-raw :pos (/ n-hist n-inst)))))

(defn reload
  "Reloads buffer of packet with new energy subbands value of 1024 samples from
  raw, preparing the packet for the next processing step."
  [packet]
  (let [{buffer :buffer raw :raw pos :pos n-inst :n-inst n-freq :n-freq} packet
        next-buffer (next-energy-subbands-buffer buffer raw n-inst n-freq)
        rest-raw (drop-raw n-inst raw)]
    (assoc packet :buffer next-buffer :raw rest-raw :pos (inc pos))))

(declare update-result)
(defn process
  "Processes given initialized packet and returns detection result.
  result is a 2-D vector that consists of detected beat instance indices.  Each
  1-D vectors in the result match with n-freq frequency subbands in the same
  order."
  [packet result]
  (if (nil? (second (:raw packet))) ; If length of raw gets smaller than 1024, FFT becomes impossible
                                    ; So simply disregard last chunk of raw
    (do (println "passing result...") result) ; FIXME stackoverflow!!!
    (recur (reload packet) ; future packet
           (update-result result (determine-beat packet)
                          (do (println "Processing" (:pos packet))
                              (println "Current raw count:" (count (:raw packet)))
                              (println "Current result count:" (count (first result)))
                              (println "result class:" (class result))
                              (:pos packet))))))

(defn- update-result
  "Conjoins result with 'binary' result vector returned from determine-beat."
  [result binary pos]
  (do (println "in update-result....") (map into result (replace {true [pos] false []} binary))))

(defn start
  "Starts frequency selected beat detection algorithm using given Packet
  data."
  [packet]
  (let [initial-result (vec (repeat (:n-freq packet) []))]
    (process (initialize packet) initial-result)))
