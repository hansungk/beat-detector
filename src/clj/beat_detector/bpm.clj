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
  [times interval]
  (let [best-corr (autocorrelate times interval)
        index-zip (map vector best-corr (range))]
    (mapv times (reduce (fn [acc [x y]]
                          (if (zero? x) (conj acc y) acc))
                        [] index-zip))))

(defn candidate-major-beats
  "Finds the longest major beats candidate chain and constructs them into a
  vector."
  [times pivot interval]
  (loop [dst [] ^long cand pivot]
    (if (<= cand (+ C (last times)))
      (let [found (find-near cand times)]
        (if (<= (Math/abs (- cand found)) 4) ; FIXME hardcoded
          (recur (conj dst found) (+ found interval))
          (recur dst (+ cand interval))))
      dst)))

(defn find-major-beats-segment
  "Tries candidate-major-beats using times as pivots and finds a long enough
  major beats chain."
  [times interval]
  (let [pivots (patterned-beats times interval)
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

(defn find-major-beats
  [times]
  (let [interval (estimated-interval times)
        segments (reduce (fn [xs y]
                           (let [xs (vec xs)
                                 x (if (empty? xs) y (last (last xs)))]
                             (if (> (- y x) (* 4 interval)) ; New segment
                               (conj xs (vector y))
                               (conj (vec (butlast xs))
                                     (conj (vec (last xs)) y))))) [] times)]
    (mapcat #(find-major-beats-segment % interval) segments)))

(defn determine-bpm ; SLOW
  [times]
  (let [majors (find-major-beats times)
        interval (estimated-interval times) ; FIXME double computation
        intervals (map #(- (second %) (first %)) (partition 2 1 majors))]
    (* 4 (/ (/ (* 60 44100.0) 1024.0)
            (average
              (reduce (fn [xs y]
                        (if (> (- y interval) (* 0.25 interval)) ; New segment
                          xs
                          (conj xs y))) [] intervals)))))) ; exact interval

(comment (defn determine-bpm ; SLOW
  [times]
  (let [majors (find-major-beats times)
        interval (estimated-interval times)
        ^double duration (- (last majors) (first majors))
        n-beats (Math/round (/ duration interval))
        exact-interval (/ duration n-beats)
        _ (println "Exact interval: " exact-interval)]
    (* 4 (/ (/ (* 60 44100.0) 1024.0) exact-interval)))))
