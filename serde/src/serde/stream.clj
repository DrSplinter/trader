(ns serde.stream
  (:require
    [serde.bytes :as b]))


(defn- stream-read-byte!
  [stream]
  (let [b (.read stream)]
    (when (= b -1)
      (throw (ex-info "unexpected end of stream" {:EOF true})))
    b))


(defn- stream-write-byte!
  [stream val]
  (.write stream (int val)))


(defn stream->src
  ([type stream]
   (stream->src type :little-endian stream))
  ([type endianness stream]
   (let [read-byte! #(stream-read-byte! stream)
         read-bytes-le! (b/read-byte!->le-read-bytes! read-byte!)
         read-bytes-be! (b/read-byte!->be-read-bytes! read-byte!)]
     (case endianness
       :little-endian (b/src type read-bytes-le!)
       :big-endian (b/src type read-bytes-be!)))))


(defn stream->dest
  ([type stream]
   (stream->dest type :little-endian stream))
  ([type endianness stream]
   (let [write-byte! #(stream-write-byte! stream %)
         write-bytes-le! (b/write-byte!->le-write-bytes! write-byte!)
         write-bytes-be! (b/write-byte!->be-write-bytes! write-byte!)]
     (case endianness
       :little-endian (b/dest type write-bytes-le!)
       :big-endian (b/dest type write-bytes-be!)))))
