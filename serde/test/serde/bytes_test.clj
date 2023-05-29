(ns serde.bytes-test
  (:require [clojure.test :refer [deftest testing are]]
            [serde.bytes :as sut]

            [clojure.instant :as inst]
            [java-time :as time]
            [serde.core :as s]))

(defn serde! [spec val]
  (let [snk (sut/memsnk)]
    (sut/ser! spec snk val)
    (let [src (-> snk sut/memsnk-bytes sut/collsrc)]
      (sut/de! spec src))))

(deftest de!-is-inverse-of-ser!
  (testing "numbers"
    (are [spec val]
         (= val (serde! spec val))
      s/byte 123
      s/byte -123
      s/int 512
      s/int -512
      s/long 32010
      s/long -32010))

  (testing "floats"
    (are [spec val]
         (Float/compare val (serde! spec val))
      s/float 3.1415
      s/float -3.12345))

  (testing "doubles"
    (are [spec val]
         (Double/compare val (serde! spec val))
      s/double 3.141514
      s/double -3.1415123))

  (testing "compounds"
    (are [spec val]
         (= val (serde! spec val))
      s/boolean true
      s/boolean false

      s/time (apply time/local-date-time (take 6 (inst/parse-timestamp vector "2001")))

      (s/vector s/byte s/int) [-10 666]

      (s/list s/byte) '(1 2 -3 -100)

      s/string "done 666"

      (s/choice [:byte s/byte]
                [:string s/string])
      [:string "done"]

      (s/choice [:byte s/byte]
                [:string s/string])
      [:byte 20]

      (s/map [:name s/string]
             [:age s/int]
             [:kids (s/list s/string)])
      {:name "John"
       :age 34
       :kids ["Hannah"]})))
