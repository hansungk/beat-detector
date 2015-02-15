(ns beat-detector.util-test
  (:require [clojure.test :refer :all]
            [beat-detector.util :refer :all :as util]))

(deftest conj-data-test
  (testing "conj-data test"
    (testing "if original is not empty"
      (is (= (util/conj-data [[1 2] [1 2]] [[3 4] [3 4]]) [[1 2 3 4] [1 2 3 4]])))
    (testing "if original is empty"
      (is (= (util/conj-data [] [[3 4] [3 4]]) [[3 4] [3 4]])))))

(deftest take-raw-test
  (testing "take-raw test"
    (testing "when n is zero"
      (is (= (util/take-raw 0 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8])))
             [])))
    (testing "when n is smaller than chunk size"
      (is (= (util/take-raw 3 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8])))
             [[1 2 3] [1 2 3]] ))))
    (testing "when n is larger than chunk size"
      (is (= (util/take-raw 5 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8])))
             [[1 2 3 4 5] [1 2 3 4 5]])))
    (testing "when n is same with chunk size"
      (is (= (util/take-raw 4 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8])))
             [[1 2 3 4] [1 2 3 4]])))
    (testing "when n is larger than size of raw"
      (is (= (util/take-raw 9 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8])))
             [[1 2 3 4 5 6 7 8] [1 2 3 4 5 6 7 8]]))))

(deftest drop-raw-test
  (testing "drop-raw test"
    (testing "when n is smaller than chunk size"
      (is (= (util/drop-raw 3 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]])
             [[[4] [4]] [[5 6 7 8] [5 6 7 8]]])))
    (testing "when n is larger than chunk size"
      (is (= (util/drop-raw 5 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]])
             [[[6 7 8] [6 7 8]]])))
    (testing "when n is same with chunk size"
      (is (= (util/drop-raw 4 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]])
             [[[5 6 7 8] [5 6 7 8]]])))
    (testing "when n is total size of raw"
      (is (= (util/drop-raw 8 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]])
             [])))
    (testing "when n is larger than size of raw"
      (is (= (util/drop-raw 9 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]])
             [])))))

(deftest average-test
  (is (= (average [1.9 3 4 5 6.1]) 4.0)))

(deftest variance-avg-test
  (is (= (variance-avg [1 3 5 7 9]) (double (/ (+ 16 4 4 16) 5)))))

(deftest sumsq-test
  (is (= (sumsq 3 4) 25)))

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

(comment (deftest find-major-beats-test
  (testing "general"
    (is (= (find-major-beats times-ideal)
           times-ideal)))))
