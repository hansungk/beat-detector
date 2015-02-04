(ns beat-detector.simplebd-test
  (:use [beat-detector.simplebd])
  (:require [clojure.test :refer :all]
            [beat-detector.core :only ->Packet :as core]))

(deftest sound-energy-test
  (is (= (sound-energy [[3 5] [4 12]]) (+ 25 169)))
  (is (= (sound-energy []) 0)))

(def ^:dynamic *raw* '(([1 2 3 4] [0 0 0 0]) ([5 6 7 8] [0 0 0 0])))

(deftest gen-energy-buffer-test
  (testing "general"
    (is (= (gen-energy-buffer *raw* 2 8)
           [5 25 61 113])))
  (testing "when history is longer than raw"
    (is (= (gen-energy-buffer *raw* 3 9)
           [14 77 113])))
  (testing "when instance is longer than raw"
    (is (= (gen-energy-buffer *raw* 9 9)
           [204]))))

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


(deftest prepare-test
  (testing "general"
    (let [packet (core/->Packet nil *raw* 2 6)]
      (is (= (take 2 (vals (prepare packet)))
             '([5 25 61] ([[7 8] [0 0]]))))))
  (testing "when raw is completely consumed"
    (let [packet (core/->Packet nil *raw* 2 8)]
      (is (= (take 2 (vals (prepare packet)))
             '([5 25 61 113] ()))))))

(deftest update-test
  (testing "general"
      (let [packet (prepare (core/->Packet nil *raw* 2 4))
            {buffer :buffer raw :raw} (update packet)]
        (is (= buffer [25 61]))
        (is (= raw '([[7 8] [0 0]])))))
  (testing "when raw depletes on update"
      (let [packet (prepare (core/->Packet nil *raw* 2 6))
            {buffer :buffer raw :raw} (update packet)]
        (is (= buffer [25 61 113]))
        (is (= raw '())))))
