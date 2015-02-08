(ns beat-detector.freqbd-test
  (:use [beat-detector.freqbd])
  (:require [clojure.test :refer :all]
            [beat-detector.core :as core]))

(def ^:dynamic *raw* core/*raw-data*)

(deftest peek-fft-buffer-test
  (testing "just check size"
    (is (= (count (peek-fft-buffer *raw* 1024 32)) 32))))

(deftest divide-test
  (testing "general"
    (is (= (divide [1 2 3 4 5 6 7 8] 4)
           [3 7 11 15]))
    (is (= (divide [1 2 3 4 5 6 7 8] 2)
           [10 26]))))

(deftest gen-energy-subbands-buffer-test
  (testing "check subband height"
    (is (= (count (time (gen-energy-subbands-buffer *raw* 1024 44032 32)))
           31)))
  (testing "check subband width"
    (is (= (count (first (gen-energy-subbands-buffer *raw* 1024 44032 32)))
           43))))

(comment (deftest next-energy-subbands-buffer-test
  (testing "general"
    (is (= (next-energy-subbands-buffer [0 1 2 3] *raw* 2)
           [1 2 3 5])))
  (testing "when there are insufficient datas (< instance-num) in raw"
    (is (= (next-energy-subbands-buffer [0 1 2 3] *raw* 9)
           [1 2 3 204])))
  (testing "when there are no more remaining raw"
    (is (= (next-energy-subbands-buffer [0 1 2 3] '() 2)
           [1 2 3 0])))
  (testing "when given buffer is empty (which should not be)"
    (is (= (next-energy-subbands-buffer [] *raw* 2)
           nil)))))


(comment (deftest initialize-test
  (testing "general"
    (let [packet (core/->Packet nil *raw* 0 2 6 nil)
          {buffer :buffer raw :raw pos :pos} (initialize packet)]
      (is (= buffer [5 25 61]))
      (is (= raw '([[7 8] [0 0]])))
      (is (= pos 3))))
  (testing "when raw is completely consumed"
    (let [packet (core/->Packet nil *raw* 0 2 8 nil)]
      (is (= (take 2 (vals (initialize packet)))
             '([5 25 61 113] ())))))))
