(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]
            [beat-detector.simplebd :refer :all :as simplebd]))

(def ^:dynamic *sound* (dynne/read-sound "sample-medium.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *sound-history-num* 44032)
(def ^:dynamic *instance-num* 1024)
(def ^:dynamic *energy-history-num* 43)
(def ^:dynamic *chunk-size* 10000)
(def ^:dynamic *click* (dynne/sinusoid 0.01 840))

; A Packet is a bundle data that contains all the necessary informations
; that it can be immediately processed by beat detection; i.e. current
; energy history buffer, raw datas that appends those stored in energy
; buffer, position of current instance, num of samples in one instance,
; and num of samples in one sound history buffer.

(defrecord Packet [buffer raw pos n-inst n-hist])

(defn times->clicks
  [times]
  (map (partial dynne/timeshift *click*) times))

(defn instances->times
  [instances]
  (map (fn [x] (* (/ *instance-num* 44100) (dec x))) instances))

(defn simplebd
  []
  (let [packet (->Packet nil *raw-data* nil *instance-num* *sound-history-num*)]
    (simplebd/start packet)))

(defn clicks
  []
  (reduce dynne/mix (times->clicks (instances->times (simplebd)))))

(defn save-clicks
  []
  (dynne/save (clicks) "clicks.wav" 44100))

(defn core
  []
  (dynne/mix *sound* (dynne/->stereo (clicks))))
