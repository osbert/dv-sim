(ns one.sample.test.dvorak
  (:use [clojure.test]
        [one.sample.dvorak]))

(deftest test-convert-to-dvorak
  (is (= (convert-to-dvorak "osbert")
         "s;ndok")))

(deftest test-convert-to-dvorak-uppercase
  (is (= (convert-to-dvorak "Osbert")
         "S;ndok")))

(deftest test-reversible
  (is (= "osbert"
          (simulate-dvorak (convert-to-dvorak "osbert")))))

(deftest test-no-mapping-found
  (is (= "1"
         (char-replace {} "1"))))

(deftest test-no-mapping-found-lowercase
  (is (= "b"
         (char-replace {} "b"))))