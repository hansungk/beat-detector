(ns beat-detector.bpm-test
  (:require [clojure.test :refer :all]
            [beat-detector.bpm :refer :all]))

(def times-ideal [0 50 100 150 200 250 300 350])

(deftest estimated-interval-test
  (testing "exact factors"
    (is (= (estimated-interval times-ideal)
           49))))

(deftest candidate-major-beats-test
  (testing "find all success"
    (is (= (candidate-major-beats [0 5 9 10 12 15 20 25 30 35] 5 5)
           [5 10 15 20 25 30 35])))
  (testing "several failed find"
    (is (= (candidate-major-beats [0 5 9 10 12 20 25 33] 0 5)
           [0 5 10 20 25 33])))
  (testing "exact match"
    (is (= (candidate-major-beats times-ideal 0 50)
           times-ideal))))
