(ns slack-sdk-clj.util
  (:require [clojure.java.data :as j]
            [clojure.java.data.builder :as jb]))

(defn invoke-builder
  [^Class c]
  (-> c
      (.getMethod "builder" (make-array Class 0))
      (.invoke nil (object-array 0))))

(defn from-builder
  "Generic helper for any builder-based class."
  [^Class c props]
  (let [builder (invoke-builder c)]
    (jb/to-java c builder props {})))

(defn map->obj
  "Single entry-point for converting maps to Java objects.
   Tries the (extensible) `clojure.java.data /to-java`,
   falling back to the Builder pattern which will succeed."
  [t arg]
  (try
    (j/to-java t arg)
    (catch Exception _
      (from-builder t arg))))
