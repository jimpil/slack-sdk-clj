(ns slack-sdk-clj.util-test
  (:require [clojure.test :refer :all]
            [slack-sdk-clj.util :as util])
  (:import [com.slack.api.methods.request.chat
            ChatPostMessageRequest]
           [com.slack.api.methods.request.files FilesUploadV2Request]))

(defmacro map->obj-assertion
  [c m]
  `(testing (.getSimpleName ~c)
     (is (instance? ~c (util/map->obj ~c ~m)))))

(deftest map->obj-tests
  (testing "SlackApiRequest"
    (map->obj-assertion
      ChatPostMessageRequest
      {:channel "C1234567"
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
                                 "bar" :bar}}})
    (map->obj-assertion
      FilesUploadV2Request
      {:uploadFiles [{:filename "..."
                      :fileData (byte-array [1 2 3])
                      ;:file (io/file "")
                      }]
       :channel "C1234567"
       :snippetType "json"
       :initialComment "the files you requested"})
    )
  )
