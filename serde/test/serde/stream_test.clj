(ns serde.stream-test
  (:require
    [clojure.instant :as inst]
    [clojure.test :refer [deftest are]]
    [java-time :as time]
    [serde.core :as s]
    [serde.stream :as sut]
    [serde.type :as t]))


(defn make-conn
  [type]
  (let [out (java.io.PipedOutputStream.)
        in (java.io.PipedInputStream. out)]
    (s/conn (sut/stream->src type in)
            (sut/stream->dest type out))))


;; TODO: make value generator based on type and rewrite to test.check

(deftest write-read-are-inverse
  (are [type val]
       (let [conn! (make-conn type)]
         (conn! val)
         (= val (conn!)))
    t/byte 3
    t/bool true
    t/bool false
    t/double 1.1
    t/long 1
    t/int 1
    t/str "test"
    t/time (apply time/local-date-time (take 6 (inst/parse-timestamp vector "2001")))

    (t/vec t/long t/double t/str) [1 1.1 "a"]
    (t/list t/long) [1 2]
    (t/list t/str) (list "1" "x")
    (t/list t/long) (range 3)
    (t/choice [:a t/int] [:b t/str]) [:b "aboj"]
    (t/choice [:a t/int] [:b t/str]) [:a 3]

    ;; (t/seq t/byte) (range 10)  ; TODO: How do we close the stream? 

    (t/map [:name t/str]
           [:age t/int]
           [:kids (t/list t/str)])
    {:name "John"
     :age 34
     :kids ["Hannah"]}))
