(ns serde.type
  "Types for memory efficient ser/de.
   
   Types are common clojure types and data structures. One exception 
   is choice which is now a value `[key value]` where key is some kind
   of tag that dictates the type of value."
  (:refer-clojure :exclude [vec map seq list str byte int long double time])
  (:require
    [clojure.core :as core]))


(def byte :byte)
(def bool :bool)
(def int :int)
(def long :long)
(def double :double)
(def time :time)
(def str :str)


(defn list
  [type]
  [:list type])


(defn seq
  [type]
  [:seq type])


(defn vec
  [& types]
  [:vec types])


(defn map
  [& keytypes]
  [:map keytypes])


(defn choice
  [& keytypes]
  [:choice keytypes])


(defn dispatch
  [type & _]
  (if (vector? type) (first type) type))
