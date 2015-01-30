(ns beat-detector.core
  (:require [dynne.sampled-sound :refer :all :as dynne]))

(def ^:dynamic *sound* (dynne/read-sound "sample.wav"))
(def ^:dynamic *raw-data* (dynne/chunks *sound* 44100))
(def ^:dynamic *sound-history-num* 44032)
(def ^:dynamic *instance-num* 1024)
(def ^:dynamic *energy-history-num* 43)
(def ^:dynamic *chunk-size* 10000)
