(ns serde.core
  (:refer-clojure :exclude [byte boolean int long float double time map vector list])
  (:require [clojure.core :as core]))

(defrecord ByteSpec [])

(def byte
  "Specification for byte number."
  (->ByteSpec))

(defrecord BoolSpec [])

(def boolean
  "Specification for boolean."
  (->BoolSpec))

(defrecord IntSpec [])

(def int
  "Specification for integer number."
  (->IntSpec))

(defrecord LongSpec [])

(def long
  "Specification for long number."
  (->LongSpec))

(defrecord FloatSpec [])

(def float
  "Specification for float number."
  (->FloatSpec))

(defrecord DoubleSpec [])

(def double
  "Specification for double number."
  (->DoubleSpec))

(defrecord TimeSpec [])

(def time
  "Specification for time."
  (->TimeSpec))

(defrecord ListSpec [spec])

(defn list
  "Specification for fixed size list of values."
  [spec]
  (->ListSpec spec))

(defrecord StringSpec [])

(def string
  "Specification for string value."
  (->StringSpec))

(defrecord VectorSpec [specs])

(defn vector
  "Specification for fixed size vector."
  [& specs]
  (->VectorSpec specs))

(defrecord ChoiceSpec [keyspecs])

(defn choice
  "Specification for choice from fixed number of tagged things.

  Choice specifies a vector of tag and value.

  Example:
  (def payment (choice [:cash nil] [:card long]))
  "
  [& keyspecs]
  (->ChoiceSpec (vec keyspecs)))
(defrecord MapSpec [keyspecs])

(defn map
  [& keyspecs]
  (->MapSpec keyspecs))
