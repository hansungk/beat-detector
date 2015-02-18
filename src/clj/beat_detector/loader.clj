(ns beat-detector.loader
  (:require [dynne.sampled-sound :as dynne]))

(def sound (dynne/read-sound "subterranean-athird.wav"))
(def raw-data (dynne/chunks sound 44100))
(def n-hist 44032)
(def n-inst 1024)
(def n-freq 64)
(def chunk-size (count (ffirst raw-data)))
(def click (dynne/sinusoid 0.01 840))
(def duration-smpl
  (+ (* chunk-size (dec (count raw-data))) (count (first (last raw-data)))))
(def duration-inst (long (/ duration-smpl n-inst)))