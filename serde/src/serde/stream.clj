(ns serde.stream
  (:import [serde.bytes Source Sink]))

(defn- stream-read-byte!
  [stream]
  (let [b (.read stream)]
    (when (= b -1)
      (throw (ex-info "unexpected end of stream" {:EOF true})))
    b))

(defn- stream-write-byte!
  [stream val]
  (.write stream (int val)))

(defrecord StreamSource [stream]
  Source
  (read-bytes! [src n]
    (->> (repeatedly #(stream-read-byte! (:stream src)))
         (take n))))

(defrecord StreamSink [stream]
  Sink
  (write-bytes! [snk bytes]
    (->> bytes
         (run! #(stream-write-byte! (:stream snk) %)))))

(defn stream->src [stream] (->StreamSource stream))

(defn stream->snk [stream] (->StreamSink stream))
