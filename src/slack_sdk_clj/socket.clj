(ns slack-sdk-clj.socket
  (:require [slack-sdk-clj.clients :as clients])
  (:import [com.slack.api.socket_mode SocketModeClient]
           [com.slack.api.socket_mode.listener EnvelopeListener WebSocketMessageListener]
           [com.slack.api.socket_mode.request SocketModeEnvelope]
           [com.slack.api.socket_mode.response AckResponse]))

(defn- with-ws-message-listeners
  ^SocketModeClient [client listeners]
  (reduce
    (memfn ^SocketModeClient addWebSocketMessageListener)
    client
    listeners))

(defn- with-ws-error-listeners
  ^SocketModeClient [client listeners]
  (reduce
    (memfn ^SocketModeClient addWebSocketErrorListener)
    client
    listeners))

(defn- envelope-listener
  ^EnvelopeListener [^SocketModeClient client f!]
  (fn [^SocketModeEnvelope envelope]
    (and (-> envelope .getPayload .toString f!) ;; handle the payload first
         (->> (-> (AckResponse/builder)         ;; then acknowledge conditionally
                  (.envelopeId (.getEnvelopeId envelope))
                  .build)
              (.sendSocketModeResponse client)))))

(defn- with-envelope-listeners
  ^SocketModeClient [client listeners]
  (reduce
    (fn [^SocketModeClient client listener]
      (doto client
        (.addEventsApiEnvelopeListener
          (envelope-listener client listener))))
    client
    listeners))

(defn- with-slash-commands-listeners
  ^SocketModeClient [client listeners]
  (reduce
    (fn [^SocketModeClient client listener]
      (doto client
        (.addSlashCommandsEnvelopeListener
          (envelope-listener client listener))))
    client
    listeners))
;;----------------------------------------------
(defn with-listeners
  [^SocketModeClient client {:socket.listeners/keys [ws-msg ws-err events slash-commands]}]
  (cond-> client
          (seq ws-msg)          (with-ws-message-listeners ws-msg)
          (seq ws-err)          (with-ws-error-listeners   ws-err)
          (seq events)          (with-envelope-listeners   events)
          (seq slash-commands)  (with-slash-commands-listeners slash-commands)))

(defn connect!    ^SocketModeClient [^SocketModeClient client] (doto client (.connect)))
(defn disconnect! ^SocketModeClient [^SocketModeClient client] (doto client (.disconnect)))
(defn close!                        [^SocketModeClient client] (.close client))
