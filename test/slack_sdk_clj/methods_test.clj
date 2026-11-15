(ns slack-sdk-clj.methods-test
  (:require [clojure.java.data :as j]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer :all]
            [slack-sdk-clj.clients :as clients]
            [slack-sdk-clj.methods :as methods]
            [slack-sdk-clj.util :as util])
  (:import [com.slack.api Slack]
           [com.slack.api.methods SlackFilesUploadV2Exception]
           [com.slack.api.methods.response.files FilesGetUploadURLExternalResponse]
           [java.lang.reflect Method ParameterizedType]))

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

(defmacro method-assertion
  [client method-name req resp-fn]
  (let [f (resolve (symbol "slack-sdk-clj.methods" method-name))]
    `(testing ~method-name
       (is (~resp-fn (~f ~client ~req))))))

(def contains-ok? #(contains? % :ok))

(deftest method-tests
  (let [slack  (Slack/getInstance)
        client (clients/methods-client slack)]

    (let [req {:foo "bar"}]
      (method-assertion client "apiTest" req :ok))

    (let [req {:channel "C1234567"
               :text "hello"
               :blocks [{:type "section"
                         :markdownText "*Please select a restaurant:*"}
                        {:type "divider"}
                        {:type "actions"
                         :elements [{:type "button"
                                     :text "button test"
                                     :value "v1"}]}]
               :linkNames true
               :unfurlLinks true
               :metadata {:eventType "type1"
                          :eventPayload {"foo" 1
                                         "bar" :bar}}}]
      (method-assertion client "chatPostMessage" req contains-ok?))

    (let [req {:uploadFiles [{:filename "..."
                              :fileData (byte-array [1 2 3])
                              ;:file (io/file "")
                              }]
               :channel "C1234567"
               :snippetType "json"
               :initialComment "the files you requested"}]
      (testing "filesUploadV2"
        (is (try
              (methods/filesUploadV2 client req)
              (catch SlackFilesUploadV2Exception e
                ;;the true error is buried inside the first response object
                (some
                  (fn [^FilesGetUploadURLExternalResponse resp]
                    (some-> (.getError resp)
                            (= "invalid_auth")))
                  (.getGetURLResponses e)))))))

    (let [req {:teamId "T1234"
               :limit 500
               :cursor "..."
               :excludeArchived true
               :types ["public_channel" "private_channel" "mpim" "im"]}]
      (method-assertion client "conversationsList" req contains-ok?))

    )
  )
