(ns serde.bytes
  "Ser/de to/from sequence of bytes."
  (:require
    [serde.type :as t])
  (:import
    (java.time
      LocalDateTime
      ZoneOffset)))


(defmulti ^:private read! t/dispatch)
(defmulti ^:private write! t/dispatch)


(defn src
  [type read-bytes!]
  (memoize #(read! type read-bytes!)))


(defn dest
  [type write-bytes!]
  (memoize #(write! type write-bytes! %)))


(defn- read-bytes!
  [s n]
  (s n))


(defn- write-bytes!
  [d n val]
  (d n val))


;;
;; Endianness
;;

(defn- read-byte!->read-bytes!
  [read-byte! count->offsets]
  (fn [count]
    (let [bytes (repeatedly read-byte!)
          offsets (count->offsets count)]
      (->> (map bit-shift-left bytes offsets)
           (take count)
           (reduce +)))))


(defn- write-byte!->write-bytes!
  [write-byte! count->offsets]
  (fn [count val]
    (let [byte-by-offset #(bit-and 0xFF (bit-shift-right val %))]
      (->> (count->offsets count)
           (map byte-by-offset)
           (take count)
           (map write-byte!)
           dorun))))


(defn- count->le-offsets
  [_]
  (iterate #(+ % 8) 0))


(defn- count->be-offsets
  [n]
  (iterate #(- % 8) (* 8 (- n 1))))


(defn read-byte!->le-read-bytes!
  [read-byte!]
  (read-byte!->read-bytes! read-byte! count->le-offsets))


(defn write-byte!->le-write-bytes!
  [write-byte!]
  (write-byte!->write-bytes! write-byte! count->le-offsets))


(defn read-byte!->be-read-bytes!
  [read-byte!]
  (read-byte!->read-bytes! read-byte! count->be-offsets))


(defn write-byte!->be-write-bytes!
  [write-byte!]
  (write-byte!->write-bytes! write-byte! count->be-offsets))


;;
;; Primitives
;;

(defmethod read! :byte [_ s]
  (->> (read-bytes! s 1) byte))


(defmethod write! :byte [_ s val]
  (->> val byte (write-bytes! s 1)))


(defmethod read! :int [_ s]
  (->> (read-bytes! s 4) int))


(defmethod write! :int [_ s val]
  (->> val int (write-bytes! s 4)))


(defmethod read! :long [_ s]
  (->> (read-bytes! s 8) long))


(defmethod write! :long [_ s val]
  (->> val long (write-bytes! s 8)))


(defmethod read! :double [_ s]
  (->> (read! :long s) (Double/longBitsToDouble)))


(defmethod write! :double [_ s val]
  (->> val double Double/doubleToLongBits (write! :long s)))


(defmethod read! :bool [_ s]
  (= (read! :byte s) 1))


(defmethod write! :bool [_ s val]
  (write! :byte s (if val 1 0)))


(defmethod read! :time [_ s]
  (LocalDateTime/ofEpochSecond (read! :long s) 0 ZoneOffset/UTC))


(defmethod write! :time [_ s val]
  (write! :long s (.toEpochSecond val ZoneOffset/UTC)))


(defmethod read! :str [_ s]
  (->> (read! [:list :byte] s)
       (map char)
       (apply str)))


(defmethod write! :str [_ s val]
  (write! [:list :byte] s val))


;;
;; Combinators
;;

(defmethod read! :vec [[_ types] s]
  (mapv #(read! % s) types))


(defmethod write! :vec [[_ types] s val]
  (doseq [[type val] (map vector types val)]
    (write! type s val)))


(defmethod read! :choice [[_ keytypes] s]
  (let [idx (read! :int s)
        [key type] (nth keytypes idx)]
    [key (read! type s)]))


(defmethod write! :choice [[_ keytypes] s c]
  (let [[[idx [_ type]] & _]
        (->> keytypes
             (map-indexed vector)
             (filter (fn [[_ [k _]]] (= k (first c)))))]
    (write! :int s idx)
    (write! type s (second c))))


(defmethod read! :list [[_ type] s]
  (let [n (read! :int s)]
    (->> (read! [:seq type] s)
         (take n)
         doall)))


(defmethod write! :list [[_ type] s vals]
  (write! :int s (count vals))
  (write! [:seq type] s vals))


(defmethod read! :seq [[_ type] s]
  (->> (repeatedly #(try (read! type s) (catch Exception _)))
       (take-while identity)))


(defmethod write! :seq [[_ type] s vals]
  (doseq [val vals]
    (write! type s val)))


(defmethod read! :map [[_ keytypes] s]
  (let [types (map second keytypes)
        keys (map first keytypes)]
    (->> (read! [:vec types] s)
         (map vector keys)
         (into {}))))


(defmethod write! :map [[_ keytypes] s val]
  (let [types (map second keytypes)
        keys (map first keytypes)
        vals (map #(get val %) keys)]
    (write! [:vec types] s vals)))
