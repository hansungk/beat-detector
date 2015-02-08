(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]
            [beat-detector.simplebd :refer :all :as simplebd]))

(def ^:dynamic *sound* (dynne/read-sound "sample.wav"))
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
; num of samples in one sound history buffer, and num of frequency
; subbands.

(defrecord Packet [buffer raw pos n-inst n-hist n-freq])

(def raw *raw-data*)

(defn testpacket
  "Returns uninitialized Packet object for testing."
  []
  (->Packet nil *raw-data* nil 1024 44032 32))

(defn times->clicks
  "Converts a vector of beat timestamps to dynne sound objects.
  Clicks are 840Hz sinusoidal waves of 0.01sec duration."
  [times]
  (map (partial dynne/timeshift *click*) times))

(defn instances->times
  "Converts a vector of beat instance indices to the real time.
  The time is when the beat starts."
  [instances]
  (map (fn [x] (* (/ *instance-num* 44100) (dec x))) instances))

(defn simplebd
  "Executes simple beat detection algorithm on the given sound source."
  []
  (let [packet (->Packet nil *raw-data* nil *instance-num* *sound-history-num* nil)]
    (simplebd/start packet)))

(defn clicks
  "Returns dynne sound objects that contains clicks that sync with
  detected beats."
  []
  (reduce dynne/mix (times->clicks (instances->times (simplebd)))))

(defn save-clicks
  "Save clicks object into wav file named clicks.wav."
  []
  (dynne/save (clicks) "clicks.wav" 44100))

(defn core
  []
  (dynne/mix *sound* (dynne/->stereo (clicks))))
