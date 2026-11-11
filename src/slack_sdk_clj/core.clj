(ns slack-sdk-clj.core
  (:require [slack-sdk-clj.methods :as methods]
            [slack-sdk-clj.token :as token]
            [slack-sdk-clj.clients :as clients])
  (:import [com.slack.api Slack SlackConfig]
           [com.slack.api.util.http SlackHttpClient]))

(defn slack-config
  ^SlackConfig  [m]
  )

(defn slack-http-client
  ^SlackHttpClient [m]
  )

(defn ^Slack slack-instance
  ""
  ([]       (Slack/getInstance))
  ([config] (Slack/getInstance (slack-config config)))
  ([config http-client] (Slack/getInstance (slack-config config) (slack-http-client http-client))))

(defonce response-ok? :ok)

(comment

  (def SLACK         (slack-instance))
  (def API-CLIENT    (clients/methods-client SLACK (token/find-in-env))) ;; define this once - then reuse it
  (def SOCKET-CLIENT (clients/socket-mode SLACK "..."))

  ;; https://docs.slack.dev/tools/java-slack-sdk/guides/web-api-client-setup/#srcmainjavaexamplejava
  (methods/apiTest API-CLIENT {:foo "bar"})
  (methods/chatPostMessage API-CLIENT {:text "hi" :channel "C..."})
  (methods/filesUploadV2 API-CLIENT {:uploadFiles [{:filename "..."
                                                    :fileData (byte-array [1 2 3])
                                                    ;:file (io/file "")
                                                    }]
                                     :channel "C..."
                                     :snippetType "json"
                                     :initialComment "hi"})

  (socket/with-listeners
    SOCKET-CLIENT
    #:socket.listeners{:ws-msg [(partial println "Received web-socket message:")]
                       :ws-err [(partial println "Received web-socket error:")]
                       :events [(partial println "Received event:")]
                       :slash-commands [(partial println "Received slash-command:")]})

  )