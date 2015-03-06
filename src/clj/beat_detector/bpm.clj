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
        ;_ (println "All beats:" times)
        ;_ (println "Patterned beats:" pivots)
        ]
    (reduce (fn [_ x]
              (let [candidates (candidate-major-beats times x interval)]
                (if (< (count candidates) (/ (count times) 10))
                  nil
                  (do
                    (println "Major beats:" candidates)
                    (reduced candidates))))) 0 pivots)))

(defn find-major-beats
  ([times]
   (find-major-beats times (estimated-interval times)))
  ([times interval]
   (let [segments (reduce (fn [xs y]
                            (if (empty? xs)
                              [[y]]
                              (let [x (peek (peek xs))]
                                (if (> (- y x) (* 4 interval)) ; New segment
                                  (conj xs (vector y))
                                  (conj (pop xs)
                                        (conj (peek xs) y)))))) [] times)]
     (mapcat #(find-major-beats-segment % interval) segments))))

; If the time distance between two adjacent major beats is longer than this
; constant * interval, the latter major beat is considered as that of a new
; segment.
(def const-segment-intv 1.25)

(defn new-segment?
  [x y]
  (> (double (- y x)) (* const-segment-intv 1.25)))

(defn determine-bpm
  "Determine exact beats per minute from given time, using find-major-beats
  determination"
  ([times]
   (let [interval (estimated-interval times)
         majors (find-major-beats times interval)]
     (determine-bpm majors interval)))
  ([majors interval]
   (let [intervals (map #(- (second %) (first %)) (partition 2 1 majors))]
     (interval->bpm
       (average
         (reduce (fn [xs y]
                   (if (> y (* const-segment-intv interval)) ; new segment
                     xs
                     (conj xs y)))
                 [] intervals)))))) ; exact interval

(comment (defn sixteenth-beats
  "Returns timestamps that are sixteenth note beats thoughout the song."
  [times]
  (let [interval (estimated-interval times)
        majors (find-major-beats times interval)
        exact-interval (bpm->interval (determine-bpm majors interval))]
    (reduce (fn [xs y]
              (let [x (peek xs)]
                (cond
                  (empty? xs) [y]
                  (new-segment? x y)
                  (conj (into xs (rest (range x y exact-interval))) y)
                  :else
                  (rest (range x y (/ (- y x) 8.0))))))))))
