(ns beat-detector.core
  (:require [dynne.sampled-sound :as dynne]
            [beat-detector.util :refer :all]
            [beat-detector.loader :as loader]
            [beat-detector.simplebd :only start :as simplebd]
            [beat-detector.freqbd :only start :as freqbd]
            [beat-detector.bpm :as bpm]))

(defn times->clicks
  "Converts a vector of beat timestamps to dynne sound objects.
  Clicks are 840Hz sinusoidal waves of 0.01sec duration."
  [times]
  (do
    (println "Timeshifting clicks...")
    (map #(dynne/timeshift loader/click %) times)))

(defn instances->times
  "Converts a vector of beat instance indices to the real time.
  The time is when the beat starts."
  [instances]
  (map (fn [x] (* (/ loader/n-inst 44100.0) (dec x))) instances))

(def ^:const chunk-overlap 4)
(defn chunk-raw
  []
  (let [n 8
        size (inc (long (/ loader/chunk-count n)))]
    (partition-all (+ chunk-overlap size) size loader/raw-data)))

(defn p-freqbd
  []
  (let [chunk-raw (chunk-raw)
        start-point (->> chunk-raw
                         (map #(/ (* (- (count %) chunk-overlap)
                                     loader/chunk-size)
                                  1024.0))
                         (reductions +)
                         (cons 0)
                         (butlast))
        result (pmap #(nth (freqbd/start %) 11) chunk-raw)]
    (vec (apply concat
                (map (fn [x y] (map #(+ % x) y)) start-point result)))))

(defn simplebd
  "Executes simple beat detection algorithm on the given sound source."
  []
  (simplebd/start loader/raw-data))

(defn freqbd
  "Executes frequency beat detection algorithm on the given sound source."
  []
  (nth (freqbd/start loader/raw-data) 11)) ; 1~7: 0~43Hz 8~13: 43~86Hz

(defn majorbd
  "Executes major beat detection algorithm on the given sound source."
  []
  (bpm/find-major-beats (p-freqbd)))

(defn bpm
  "Executes major beat detection algorithm on the given sound source."
  []
  (bpm/determine-bpm (p-freqbd)))

(defn clicks
  "Returns dynne sound objects that contains clicks that sync with
  detected beats.
  Algorithm is :simple or :freq."
  [algorithm]
  (case algorithm
    :simple (reduce dynne/mix (times->clicks (instances->times (simplebd))))
    :freq (reduce dynne/mix (times->clicks (instances->times (freqbd))))
    :pfreq (reduce dynne/mix (times->clicks (instances->times (p-freqbd))))
    :major (reduce dynne/mix (times->clicks (instances->times (majorbd))))))

(defn save-clicks
  "Save clicks object into an aptly named wav file.
  flag is :simple or :freq or :major."
  [flag]
  (let [filename (case flag
                   :simple "wavs/simple.wav"
                   :freq "wavs/freq.wav"
                   :pfreq "wavs/pfreq.wav"
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
