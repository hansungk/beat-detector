(ns beat-detector.core
  (:require [dynne.sampled-sound :as dynne]
            [beat-detector.util :refer :all]
            [beat-detector.loader :as loader]
            [beat-detector.simplebd :only start :as simplebd]
            [beat-detector.freqbd :only start :as freqbd]
            [beat-detector.bpm :as bpm]))

; A Packet is a bundle data that contains all the necessary informations
; that it can be immediately processed by beat detection; i.e. current
; energy history buffer, raw datas that appends those stored in energy
; buffer, position of current instance, num of samples in one instance,
; num of samples in one sound history buffer, and num of frequency
; subbands.
(defrecord Packet [buffer raw pos n-inst n-hist n-freq])

(defn testpacket
  "Returns uninitialized Packet object for testing."
  []
  (->Packet nil loader/raw-data nil loader/n-inst loader/n-hist loader/n-freq))

(defn times->clicks
  "Converts a vector of beat timestamps to dynne sound objects.
  Clicks are 840Hz sinusoidal waves of 0.01sec duration."
  [times]
  (do
    (println "Timeshifting clicks...")
    (map (partial dynne/timeshift loader/click) times)))

(defn instances->times
  "Converts a vector of beat instance indices to the real time.
  The time is when the beat starts."
  [instances]
  (map (fn [x] (* (/ loader/n-inst 44100) (dec x))) instances))

(defn simplebd
  "Executes simple beat detection algorithm on the given sound source."
  []
  (let [packet
        (->Packet nil loader/raw-data nil loader/n-inst loader/n-hist nil)]
    (simplebd/start packet)))

(defn freqbd
  "Executes frequency beat detection algorithm on the given sound source."
  []
  (let [packet
        (->Packet nil loader/raw-data nil loader/n-inst loader/n-hist loader/n-freq)]
    (nth (freqbd/start packet) 11))) ; 1~7: 0~43Hz 8~13: 43~86Hz

(defn majorbd
  "Executes major beat detection algorithm on the given sound source."
  []
  (bpm/find-major-beats (freqbd)))

(defn bpm
  "Executes major beat detection algorithm on the given sound source."
  []
  (bpm/determine-bpm (freqbd)))

(defn corr
  []
  (corr-zerocounts (freqbd)))

(defn interval
  []
  (bpm/estimated-interval (freqbd)))

(defn best-corr
  []
  (autocorrelate (freqbd) (interval)))

(defn clicks
  "Returns dynne sound objects that contains clicks that sync with
  detected beats.
  Algorithm is :simple or :freq."
  [algorithm]
  (case algorithm
    :simple (reduce dynne/mix (times->clicks (instances->times (simplebd))))
    :freq (reduce dynne/mix (times->clicks (instances->times (freqbd))))
    :major (reduce dynne/mix (times->clicks (instances->times (majorbd))))))

(defn save-clicks
  "Save clicks object into an aptly named wav file.
  flag is :simple or :freq or :major."
  [flag]
  (let [filename (case flag
                   :simple "wavs/simple.wav"
                   :freq "wavs/freq.wav"
                   :major "wavs/major.wav")]
    (dynne/save (clicks flag) (do (println "Saving...") filename) 44100)))

(defn setup
  []
  (do
    (save-clicks :freq)
    (save-clicks :major)
    (bpm)))

(defn core
  []
  (dynne/mix loader/sound (dynne/->stereo (clicks))))
