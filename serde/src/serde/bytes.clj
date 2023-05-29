(ns serde.bytes
  "Ser/de to/from sequence of bytes."
  (:require [serde.core :as s])
  (:import (serde.core ByteSpec
                       BoolSpec
                       IntSpec
                       LongSpec
                       FloatSpec
                       DoubleSpec
                       TimeSpec
                       ListSpec
                       StringSpec
                       VectorSpec
                       ChoiceSpec
                       MapSpec)
           (java.time ZoneOffset
                      LocalDateTime)))

;;;
;;; Protocols
;;;

(defprotocol Sink
  (write-bytes! [snk bytes]))

(defprotocol Source
  (read-bytes! [src n]))

(defprotocol Serialize
  (ser! [dt snk val]))

(defprotocol Deserialize
  (de! [dt src]))

;;
;; Simple implementation
;;

(defrecord CollSource [bytes]
  Source
  (read-bytes! [src n]
    (dosync
     (let [[bytes buffer] (split-at n @(:bytes src))]
       (ref-set (:bytes src) buffer)
       bytes))))

(defn collsrc [coll] (->CollSource (ref coll)))

(defrecord MemorySink [bytes]
  Sink
  (write-bytes! [snk bytes]
    (swap! (:bytes snk) #(reduce conj % bytes))
    nil))

(defn memsnk [] (->MemorySink (atom [])))
(defn memsnk-bytes [snk] @(:bytes snk))


;;
;; Helper functions
;;

(defn- num->le-bytes [n num]
  ;; Works incorrectly for n > 8
  (let [byte-by-offset #(bit-and 0xFF (bit-shift-right num %))]
    (->> (iterate #(+ % 8) 0)
         (map byte-by-offset)
         (take n))))

(defn- le-bytes->num [bytes]
  (->> (iterate #(+ % 8) 0)
       (map bit-shift-left bytes)
       (take (count bytes))
       (reduce +)))

(defn- read-num! [src n]
  (->> (read-bytes! src n)
       le-bytes->num))

(defn- write-num! [snk n num]
  (->> num
       (num->le-bytes n)
       (write-bytes! snk)))


;;;
;;; Specification
;;;

(extend-type ByteSpec
  Serialize
  (ser! [_ snk val] (->> val byte (write-num! snk 1)))
  Deserialize
  (de! [_ src] (->> (read-num! src 1) unchecked-byte)))

(extend-type BoolSpec
  Serialize
  (ser! [_ snk val] (->> (if val 1 0) (ser! s/byte snk)))
  Deserialize
  (de! [_ src] (= 1 (de! s/byte src))))

(extend-type IntSpec
  Deserialize
  (de! [_ src] (->> (read-num! src 4) unchecked-int))
  Serialize
  (ser! [_ snk val] (->> val int (write-num! snk 4))))

(extend-type LongSpec
  Deserialize
  (de! [_ src] (->> (read-num! src 8) unchecked-long))
  Serialize
  (ser! [_ snk val] (->> val long (write-num! snk 8))))

(extend-type FloatSpec
  Deserialize
  (de! [_ src] (->> (de! s/int src) Float/intBitsToFloat))
  Serialize
  (ser! [_ snk val] (->> val float Float/floatToIntBits (ser! s/int snk))))

(extend-type DoubleSpec
  Deserialize
  (de! [_ src] (->> (de! s/long src) Double/longBitsToDouble))
  Serialize
  (ser! [_ snk val] (->> val double Double/doubleToLongBits (ser! s/long snk))))

(extend-type TimeSpec
  Serialize
  (ser! [_ snk val] (->> (.toEpochSecond val ZoneOffset/UTC) (ser! s/long snk)))
  Deserialize
  (de! [_ src] (LocalDateTime/ofEpochSecond (de! s/long src) 0 ZoneOffset/UTC)))

(extend-type ListSpec
  Deserialize
  (de! [lst src]
    (let [count (de! s/int src)]
      (->> (repeatedly #(de! (:spec lst) src))
           (take count)
           doall)))
  Serialize
  (ser! [lst snk val]
    (ser! s/int snk (count val))
    (->> val
         (run! #(ser! (:spec lst) snk %)))))

(extend-type StringSpec
  Deserialize
  (de! [_ src]
    (->> (de! (s/list s/byte) src)
         (map char)
         (apply str)))
  Serialize
  (ser! [_ snk val]
    (ser! (s/list s/byte) snk val)))

(extend-type VectorSpec
  Serialize
  (ser! [vec snk val]
    (->> val
         (map #(ser! %1 snk %2) (:specs vec))
         (run! identity)))
  Deserialize
  (de! [vec src]
    (mapv #(de! % src) (:specs vec))))

(defn position [f vec]
  (->> vec
       (map-indexed vector)
       (filter (fn [[_ e]] (f e)))
       first
       first))

(extend-type ChoiceSpec
  Serialize
  (ser! [chs snk [tag val]]
    (let [tag-idx (position (fn [[t _]] (= t tag)) (:keyspecs chs))
          [_ tag-spec] (get (:keyspecs chs) tag-idx)]
      (ser! s/byte snk tag-idx)
      (ser! tag-spec snk val)))
  Deserialize
  (de! [chs src]
    (let [tag-idx (de! s/byte src)
          [tag tag-spec] (get (:keyspecs chs) (long tag-idx))]
      [tag (de! tag-spec src)])))

(extend-type MapSpec
  Serialize
  (ser! [spec snk m]
    (->> (:keyspecs spec)
         (run! (fn [[k s]] (ser! s snk (get m k))))))
  Deserialize
  (de! [spec src]
    (->> (:keyspecs spec)
         (map (fn [[k s]] [k (de! s src)]))
         (into {}))))
