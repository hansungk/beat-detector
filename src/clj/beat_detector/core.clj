(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]
            [beat-detector.simplebd :refer :all :as simplebd]))

(def ^:dynamic *sound* (dynne/read-sound "sample.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *sound-history-num* 44032)
(def ^:dynamic *instance-num* 1024)
(def ^:dynamic *energy-history-num* 43)
(def ^:dynamic *chunk-size* 10000)

; A Packet is a bundle data that contains all the necessary informations
; that it can be immediately processed by beat detection; i.e. current
; energy history buffer, raw datas that appends those stored in energy
; buffer, num of samples in one instance, and num of samples in one
; sound history buffer.
(defrecord Packet [buffer raw n-inst n-hist])

(defn core
  []
  (simplebd/trigger *raw-data* *instance-num* *sound-history-num*))
