(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]))

(def ^:dynamic *sound* (dynne/read-sound "sample.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *sound-history-num* 44032)
(def ^:dynamic *instance-num* 1024)
(def ^:dynamic *energy-history-num* 43)
(def ^:dynamic *chunk-size* 10000)

;(defn- initial-energy-buffer
;  "Constructs a new energy buffer from scratch and returns it."
;  [data-buffer instance-num history-num]
;  (loop [buf '()
;         data-buf data-buffer]
;    (if (< (count (first data-buf)) instance-num) ; Check if data-buffer is shorter than instance-num; that is all consumed
;      [buf data-buf]
;      (let [[data-cons data-new] (split-at-data instance-num data-buf)]
;        (recur (conj buf (sound-energy data-cons)) data-new)))))

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
