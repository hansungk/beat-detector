(ns beat-detector.simplebd-test
  (:require [clojure.test :refer :all])
  (:use [beat-detector.simplebd]))

(deftest sound-energy-test
  (is (= (sound-energy [[3 5] [4 12]]) (+ 25 169))))

(def ^:dynamic *raw* '(([1 2 3 4] [0 0 0 0]) ([5 6 7 8] [0 0 0 0])))

(deftest gen-energy-buffer-test
  (testing "general"
    (is (= (gen-energy-buffer *raw* 2 4)
           [5 25])))
  (testing "when history is longer than raw"
    (is (= (gen-energy-buffer *raw* 3 9)
           [14 77 113])))
  (testing "when instance is longer than raw"
    (is (= (gen-energy-buffer *raw* 9 9)
           [204]))))

(deftest next-energy-buffer-test
  (testing "general"
    (is (= (next-energy-buffer [0 1] *raw* 2) [1 5]))))
