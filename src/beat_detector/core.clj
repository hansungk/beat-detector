(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]))

(def *sound* (dynne/read-sound "sample.wav"))
(def *raw-data* (dynne/chunks *sound* 44100)) 
(def *chunk-count* (count *raw-data*))
(def *sound-history-num* 44032)
(def *instance-num* 1025)
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

(defn fill-energy-buffer
  "Returns a sound energy histroy buffer." ; TODO
  [data-buffer raw instance-num history-num]
  )

(defn average-local-energy
  "Returns average energy from the given energy-buffer
  (default 43-length)."
  [energy-buffer])

(defn energy-variance
  "Returns the variance of the energies from the given energy-buffer
  (default 43-length)."
  [energy-buffer])

(defn peak-threshold-factor
  "Returns the factor threshold for an energy peak to be detected as a
  beat, which is determined by the local variance energy-variance of
  sound energy."
  [energy-variance])

(defn detect-beat
  "Returns the result of beat detection, which is held by examining
  the factor between the energy of local peak and that of local average."
  [instant-energy average-local-energy threshold-factor])

(defn core
  "Program starting point"
  [])
