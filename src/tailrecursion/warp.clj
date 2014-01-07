(ns tailrecursion.warp
  "Conditionals in the exception dimension enabling faster-than-logic (FTL) travel.  Based on the Alcubierre drive."
  (:import java.util.WeakHashMap)
  (:require [clojure.walk :as walk]))
 
(def ^:dynamic *e*)
 
(def ^:private thrown (WeakHashMap.))
 
(defn- replace-throws [expr]
  (->
    #(or (and (and (seq? %) (= 'throw (first %))) `(throw! ~@(rest %))) %)
    (walk/postwalk expr)))
 
(defmacro throw! [expr]
  `(let [t# ~expr]
     (.put @#'thrown t# true)
     (throw t#)))

(defmacro rethrow [head & args]
  (let [info?      (map? head)
        info       (if info? head {})
        [fmt args] (if info? (rest args) args)]
    `(throw! (ex-info (format ~fmt ~@args) ~info *e*))))

(defmacro try* [expr & [catch' finally']]
  `(try
     ~(replace-throws expr)
     (catch Throwable e#
       (binding [*e* e#]
         (if (.get @#'thrown e#)
           (throw e#)
           ~(replace-throws catch'))))
     (finally
       ~(replace-throws finally'))))

(defmacro or*
  ([] nil)
  ([x] `(try* ~x))
  ([x & xs] `(try* ~x (or* ~@xs))))
