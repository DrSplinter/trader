(ns serde.core
  "Serialization and deserialization library.
   
   This library works with sources and destinations. You can think of them
   like deserializers and serializers that deals with only one value, i.e.,
   source of double deserializes a value on first call and memoize it. 
   Similarly it works for destination.")


(defn value->src
  [val]
  (fn [] val))


(def void-dest (fn [_]))


(defn src
  [read!]
  (memoize #(read!)))


(defn dest
  [write!]
  (memoize #(write! %)))


(defn conn
  "Connection is a source and destination at the same time."
  [src dest]
  (fn
    ([] (src))
    ([val] (dest val))))
