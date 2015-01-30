(ns beat-detector.core-test
  (:require [clojure.test :refer :all]
            [beat-detector.core :refer :all :as core]))

(deftest conj-data-test
  (testing "conj-data test"
    (testing "if original is not empty"
      (is (= (core/conj-data [[1 2] [1 2]] [[3 4] [3 4]]) [[1 2 3 4] [1 2 3 4]])))
    (testing "if original is empty"
      (is (= (core/conj-data [] [[3 4] [3 4]]) [[3 4] [3 4]])))))

(deftest take-raw-test
  (testing "take-raw test"
    (testing "when n is zero"
      (is (= (first (core/take-raw 0 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8]))))  [])))
    (testing "when n is smaller than chunk size"
      (is (= (first (core/take-raw 3 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8]))))  [[1 2 3] [1 2 3]]))))
    (testing "when n is larger than chunk size"
      (is (= (first (core/take-raw 5 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8]))))  [[1 2 3 4 5] [1 2 3 4 5]])))
    (testing "when n is same with chunk size"
      (is (= (first (core/take-raw 4 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8]))))  [[1 2 3 4] [1 2 3 4]])))
    (testing "when n is total size of raw"
      (is (= (first (core/take-raw 8 '(([1 2 3 4] [1 2 3 4]) ([5 6 7 8] [5 6 7 8]))))  [[1 2 3 4 5 6 7 8] [1 2 3 4 5 6 7 8]]))))

(deftest drop-raw-test
  (testing "drop-raw test"
    (testing "when n is smaller than chunk size"
      (is (= (core/drop-raw 3 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]]) [[[4] [4]] [[5 6 7 8] [5 6 7 8]]])))
    (testing "when n is larger than chunk size"
      (is (= (core/drop-raw 5 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]]) [[[6 7 8] [6 7 8]]])))
    (testing "when n is same with chunk size"
      (is (= (core/drop-raw 4 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]]) [[[5 6 7 8] [5 6 7 8]]])))
    (testing "when n is total size of raw"
      (is (= (core/drop-raw 8 [[[1 2 3 4] [1 2 3 4]] [[5 6 7 8] [5 6 7 8]]]) [])))))
