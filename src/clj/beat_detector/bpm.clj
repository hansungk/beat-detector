(ns beat-detector.bpm
  (:require [beat-detector.util :refer :all]))

(defn estimated-interval
  "Returns estimated interval of instances by which given beats repeat by
  a specific pattern."
  [times]
  (let [zerocounts (corr-zerocounts times)
        maxcount (apply max zerocounts)]
    (+ (.indexOf zerocounts maxcount) (quot C 2))))

(defn patterned-beats
  "Returns timestamps of patterned beats(those which yield 0 from
  autocorrelation)."
  [times]
  (let [interval (estimated-interval times)
        best-corr (autocorrelate times interval)
        index-zip (map vector best-corr (range))]
    (mapv times (reduce (fn [acc [x y]]
                          (if (zero? x) (conj acc y) acc))
                        [] index-zip))))

(defn candidate-major-beats
  "Finds candidate major beats from the given 'pivot' major beat."
  [times pivot interval]
  (loop [dst [] ^long cand pivot]
    (if (<= cand (+ C (last times)))
      (let [found (find-near cand times)]
        (if (<= (Math/abs (- cand found)) 4) ; FIXME: 4 is hardcoded
          (recur (conj dst found) (+ found interval))
          (recur dst (+ cand interval))))
      dst)))

(defn find-major-beats ; SLOW
  [times]
  (let [pivots (patterned-beats times)
        interval (estimated-interval times)
        _ (println "All beats:" times)
        _ (println "Patterned beats:" pivots)
        _ (println "Interval: " interval)]
    (reduce (fn [_ x]
              (let [_ (println "Trying" x "as pivot")
                    candidates (candidate-major-beats times x interval)]
                (if (< (count candidates) (/ (count times) 10))
                  (println "Too short candidates:" candidates)
                  (do
                    (println "Elected candidate:" candidates)
                    (reduced candidates))))) 0 pivots)))

(defn determine-bpm ; SLOW
  [times]
  (let [majors (find-major-beats times)
        interval (estimated-interval times)
        ^double duration (- (last majors) (first majors))
        n-beats (Math/round (/ duration interval))
        exact-interval (/ duration n-beats)
        _ (println "Exact interval: " exact-interval)]
    (* 4 (/ (/ (* 60 44100.0) 1024.0) exact-interval))))
