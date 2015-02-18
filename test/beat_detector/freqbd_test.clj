(ns beat-detector.freqbd-test
  (:use [beat-detector.freqbd])
  (:require [clojure.test :refer :all]
            [beat-detector.util :as util]
            [beat-detector.loader :as loader]
            [beat-detector.core :only ->Packet :as core]))

(def raw loader/raw-data)

(deftest generate-fft-buffer-test
  (testing "just check size"
    (is (= (count (generate-fft-buffer raw 1024 32)) 32))))

(deftest divide-test
  (testing "general"
    (is (= (divide [1 2 3 4 5 6 7 8] 4)
           [3 7 11 15]))
    (is (= (divide [1 2 3 4 5 6 7 8] 2)
           [10 26]))))

(def initial-esb (generate-energy-subbands-buffer raw 1024 44032 32))

(deftest generate-energy-subbands-buffer-test
  (testing "check subband height"
    (is (= (count initial-esb)
           32)))
  (testing "check subband width"
    (is (= (count (first initial-esb))
           43))))

(def second-esb (next-energy-subbands-buffer initial-esb (util/drop-raw 1024 raw) 1024 32))

(deftest next-energy-subbands-buffer-test
  (testing "check subband height"
    (is (= (count second-esb)
           32)))
  (testing "check subband width"
    (is (= (count (first second-esb))
           43)))
  (testing "check index shift by matching 1st and 2nd elem"
    (is (= (second (first initial-esb))
           (first (first second-esb))))))

(deftest initialize-test
  (testing "general"
    (let [packet (core/->Packet nil raw nil 1024 44032 32)
          {buffer :buffer raw :raw pos :pos} (initialize packet)]
      (is (= (count buffer) 32))
      (is (= (count (first buffer)) 43))
      (is (= pos 43))))
  (testing "when raw is empty"
    (let [packet (core/->Packet nil [] nil 1024 44032 32)]
      (is (nil? (initialize packet))))))
