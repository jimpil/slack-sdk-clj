(ns slack-sdk-clj.methods-test
  (:require [clojure.java.data :as j]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [slack-sdk-clj.methods :as methods]
            [slack-sdk-clj.util :as util])
  (:import [java.lang.reflect Method ParameterizedType]))

(defn builder-methods
  [^Class c]
  (try
    (->> (util/invoke-builder c) class .getMethods seq)
    (catch Exception _ nil)))

(deftest nested-builder-types-are-handled
  (let [public-methods (#'methods/api-methods)
        request-classes (->> public-methods
                             (mapcat :parameter-types)
                             (filter (comp #(str/ends-with? % "Request") name))
                             (into #{} (map resolve)))
        with-nested-builder
        (->> request-classes
             (filter
               (fn [c]
                 (some
                   (fn [^Method m]
                     (let [g (first (.getGenericParameterTypes m))]
                       (and (instance? ParameterizedType g)
                            (some
                              #(and (instance? Class %)
                                    (not (str/starts-with? (.getName ^Class %) "java.")))
                              (.getActualTypeArguments ^ParameterizedType g)))))
                   (builder-methods c))))
             (map util/class-name)
             set)]
    ;; find all Request classes with nested types (requiring further building)
    (is (empty? (->> (methods j/to-java)
                     keys
                     (remove #{:default})
                     (map (comp util/class-name first))
                     (filter #(str/starts-with? % "com.slack.api.methods.request"))
                     set
                     (set/difference with-nested-builder))))))
