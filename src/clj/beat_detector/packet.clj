(ns beat-detector.packet
  (:require [beat-detector.loader :as loader]))

; A Packet is a bundle data that contains all the necessary informations
; that it can be immediately processed by beat detection; i.e. current
; energy history buffer, raw datas that appends those stored in energy
; buffer, position of current instance, num of samples in one instance,
; num of samples in one sound history buffer, and num of frequency
; subbands.
(defrecord Packet [buffer raw pos n-inst n-hist n-freq])

(defn pack
  "Returns an empty-buffer packet containing raw."
  ([raw]
   (pack raw nil loader/n-inst loader/n-hist loader/n-freq))
  ([raw pos n-inst n-hist n-freq]
   (->Packet nil raw pos n-inst n-hist n-freq)))
