(ns beat-detector.simplebd-test
  (:use [beat-detector.simplebd])
  (:require [clojure.test :refer :all]
            [beat-detector.core :only ->Packet :as core]))

(def ^:dynamic *raw* '(([1 2 3 4] [0 0 0 0]) ([5 6 7 8] [0 0 0 0])))

(deftest next-energy-buffer-test
  (testing "general"
    (is (= (next-energy-buffer [0 1 2 3] *raw* 2)
           [1 2 3 5])))
  (testing "when there are insufficient datas (< instance-num) in raw"
    (is (= (next-energy-buffer [0 1 2 3] *raw* 9)
           [1 2 3 204])))
  (testing "when there are no more remaining raw"
    (is (= (next-energy-buffer [0 1 2 3] '() 2)
           [1 2 3 0])))
  (testing "when given buffer is empty (which should not be)"
    (is (= (next-energy-buffer [] *raw* 2)
           nil))))


(deftest initialize-test
  (testing "general"
    (let [packet (core/->Packet nil *raw* 0 2 6 nil)
          {buffer :buffer raw :raw pos :pos} (initialize packet)]
      (is (= buffer [5 25 60]))
      (is (= raw '([[7 8] [0 0]])))
      (is (= pos 3))))
  (testing "when raw is completely consumed"
    (let [packet (core/->Packet nil *raw* 0 2 8 nil)]
      (is (= (take 2 (vals (initialize packet)))
             '([5 25 61 113] ()))))))

(deftest reload-test
  (testing "general"
      (let [packet (initialize (core/->Packet nil *raw* 0 2 4 nil))
            {buffer :buffer raw :raw pos :pos} (reload packet)]
        (is (= buffer [25 61]))
        (is (= raw '([[7 8] [0 0]])))
        (is (= pos (inc (/ 4 2))))))
  (testing "when raw depletes on update"
      (let [packet (initialize (core/->Packet nil *raw* 0 2 6 nil))
            {buffer :buffer raw :raw pos :pos} (reload packet)]
        (is (= buffer [25 61 113]))
        (is (= raw '()))
        (is (= pos (inc (/ 6 2)))))))

(def ^:dynamic *rawl* '(([1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 11] [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 11]) ([1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 11] [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 11])))
