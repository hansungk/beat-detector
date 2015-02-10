(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]
            [beat-detector.simplebd :only start :as simplebd]
            [beat-detector.freqbd :only start :as freqbd]))

(def ^:dynamic *sound* (dynne/read-sound "sample-medium.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *n-hist* 44032)
(def ^:dynamic *n-inst* 1024)
(def ^:dynamic *n-freq* 64)
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
  (->Packet nil *raw-data* nil *n-inst* *n-hist* *n-freq*))

(defn times->clicks
  "Converts a vector of beat timestamps to dynne sound objects.
  Clicks are 840Hz sinusoidal waves of 0.01sec duration."
  [times]
  (do
    (println "Timeshifting clicks...")
    (map (partial dynne/timeshift *click*) times)))

(defn instances->times
  "Converts a vector of beat instance indices to the real time.
  The time is when the beat starts."
  [instances]
  (map (fn [x] (* (/ *n-inst* 44100) (dec x))) instances))

(defn simplebd
  "Executes simple beat detection algorithm on the given sound source."
  []
  (let [packet
        (->Packet nil *raw-data* nil *n-inst* *n-hist* nil)]
    (simplebd/start packet)))

(defn freqbd
  "Executes simple beat detection algorithm on the given sound source."
  []
  (let [packet
        (->Packet nil *raw-data* nil *n-inst* *n-hist* *n-freq*)]
    (nth (freqbd/start packet) 11))) ; 1~7: 0~43Hz 8~13: 43~86Hz

(defn clicks
  "Returns dynne sound objects that contains clicks that sync with
  detected beats.
  Algorithm is :simple or :freq."
  [algorithm]
  (case algorithm
    :simple (reduce dynne/mix (times->clicks (instances->times (simplebd))))
    :freq (reduce dynne/mix (times->clicks (instances->times (freqbd))))))

(defn save-clicks
  "Save clicks object into wav file named clicks.wav.
  Algorithm is :simple or :freq."
  [algorithm]
  (let [filename (case algorithm :simple "clicks-simple.wav" :freq "clicks-freq.wav")]
    (dynne/save (clicks algorithm) (do (println "Saving...") filename) 44100)))

(defn core
  []
  (dynne/mix *sound* (dynne/->stereo (clicks))))
