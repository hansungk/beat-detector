(ns beat-detector.core-test
  (:require [clojure.test :refer :all]
            [beat-detector.core :refer :all]))

(deftest fill-data-buffer-test
  (testing "fill-data-buffer test"
    (testing "when pos exceeded song length"
      (is (= (beat-detector.core/fill-data-buffer-pos ['(1.0) '(1.0)] 2000 1024)
             [['(1.0) '(1.0)] 2000])))
    (testing "when buffer is empty"
      (is (= (second (beat-detector.core/fill-data-buffer-pos ['() '()] 0 1024))
             1)))))
