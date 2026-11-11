(ns slack-sdk-clj.clients
  (:import [com.slack.api Slack]
           [com.slack.api.methods MethodsClient]
           [com.slack.api.socket_mode SocketModeClient SocketModeClient$Backend]))

(defn ^MethodsClient methods-client
  "Given a `Slack` instance, returns its `MethodsClient`.
   It is a good idea to keep this object somewhere globally,
   and reuse it. In fact, all (generated) functions in
   `slack-sdk-clj.methods` expect it as their first arg! "
  ([^Slack slack]               (.methods slack))
  ([^Slack slack token]         (.methods slack token))
  ([^Slack slack token team-id] (.methods slack token team-id)))

(defn ^SocketModeClient socket-mode
  ([^Slack slack app-token]
   (.socketMode slack app-token))
  ([^Slack slack app-token backend]
   (case backend
     :tyrus   (.socketMode slack app-token SocketModeClient$Backend/Tyrus)
     :java-ws (.socketMode slack app-token SocketModeClient$Backend/JavaWebSocket))))
