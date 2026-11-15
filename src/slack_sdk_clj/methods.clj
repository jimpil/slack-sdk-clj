(ns slack-sdk-clj.methods
  (:require [clojure.reflect :as r]
            [clojure.java.data :as j]
            [clojure.set :as set]
            [clojure.string :as str]
            [jsonista.core :as json]
            [slack-sdk-clj.util :as util])
  (:import [clojure.lang APersistentMap]
           [clojure.reflect Method]
           [com.slack.api RequestConfigurator]
           [com.slack.api.methods MethodsClient]
           [com.slack.api.methods.request.chat ChatPostEphemeralRequest ChatPostMessageRequest ChatScheduleMessageRequest ChatUnfurlRequest ChatUnfurlRequest$UnfurlDetail ChatUpdateRequest]
           [com.slack.api.methods.request.conversations ConversationsListRequest]
           [com.slack.api.methods.request.files FilesUploadV2Request FilesUploadV2Request$UploadFile]
           [com.slack.api.model Attachment ConversationType Message$Metadata]
           [java.lang.reflect ParameterizedType]
           [okhttp3 FormBody$Builder MultipartBody$Builder]))

(set! *warn-on-reflection* true)

(defn- with-json-str
  [props k rk]
  (-> props
      (assoc rk (json/write-value-as-string (get props k)))
      (dissoc k)))

(defn- chat-req*
  [props]
  (cond-> props
          (contains? props :metadata)
          (with-json-str :metadata :metadataAsString)

          (contains? props :attachments)
          (with-json-str :attachments :attachmentsAsString)

          (contains? props :blocks)
          (with-json-str :blocks :blocksAsString)))

;; some Request classes contain fields that need further building
;; teach `clojure.java.data/to-java` how to do it

(defmethod j/to-java [FilesUploadV2Request APersistentMap]
  [_ props]
  ;; the `filesUploadV2` is able to upload multiple files (via :uploadFiles),
  ;; but the param value is expected to be a list of `FilesUploadV2Request$UploadFile`
  (if (contains? props :uploadFiles)
    (->> (partial util/from-builder FilesUploadV2Request$UploadFile)
         (partial mapv)
         (update props :uploadFiles)
         (util/from-builder FilesUploadV2Request))
    props))

(defmethod j/to-java [ChatPostMessageRequest APersistentMap]
  [_ props]
  (let [req (chat-req* props)]
    (util/from-builder ChatPostMessageRequest req)))

(defmethod j/to-java [ChatUpdateRequest APersistentMap]
  [_ props]
  (let [req (chat-req* props)]
    (util/from-builder ChatUpdateRequest req)))

(defmethod j/to-java [ChatScheduleMessageRequest APersistentMap]
  [_ props]
  (let [req (chat-req* props)]
    (util/from-builder ChatScheduleMessageRequest req)))

(defmethod j/to-java [ChatPostEphemeralRequest APersistentMap]
  [_ props]
  (let [req (chat-req* props)]
    (util/from-builder ChatPostEphemeralRequest req)))

(defmethod j/to-java [ChatUnfurlRequest APersistentMap]
  [_ props]
  (let [req (cond-> props
                    (contains? props :userAuthBlocks)
                    (with-json-str :userAuthBlocks :rawUserAuthBlocks)

                    (contains? props :unfurls)
                    (with-json-str :unfurls :rawUnfurls))]
    (util/from-builder ChatUnfurlRequest req)))

(defmethod j/to-java [ConversationsListRequest APersistentMap]
  [_ props]
  (let [req (cond-> props
                    (contains? props :types)
                    (update :types (partial mapv (fn [t] (ConversationType/valueOf (str/upper-case t))))))]
    (util/from-builder ConversationsListRequest req)))

;; ----------
;; ----------

(def ^:private public-instance-method?
  (every-pred
    (partial instance? Method)
    (comp #(contains? % :public) :flags)
    (comp #(not (contains? % :static)) :flags)))

(defn- api-methods []
  (->> MethodsClient
       r/reflect
       :members
       (filter public-instance-method?)
       (remove (comp #{`FormBody$Builder
                       `MultipartBody$Builder
                       `RequestConfigurator}
                     first
                     :parameter-types))))

(defn- gen1
  "Returns the wrapper-fn for the given `com.slack.api.methods.MethodsClient` method."
  [{:keys [name parameter-types return-type]}]
  `(defn ~name
     ~(str "Invokes the '" name "' Slack api method." \newline
           "Expects a `MethodsClient` and a request-map (per properties of `"
           (first parameter-types) "`)"
           (if (< 1 (count parameter-types))
             (str ", followed by `" (vec (next parameter-types)) "`.")
             ".") \newline
           "Returns a response-map (per properties of `" return-type "`).")
     [~'client & ~'args]
     (j/from-java
       (. ~(with-meta 'client {:tag `MethodsClient})
          ~name
          ~@(map-indexed
              (fn [i# t#]
                (let [arg# `(nth ~'args ~i#)]
                  (if (str/ends-with? (clojure.core/name t#) "Request") ;; SlackApiRequest
                    (with-meta `(util/map->obj ~t# ~arg#) {:tag t#})
                   arg#))) ;; plain String probably
              parameter-types)))))

(defmacro ^:private gen-all! []
  `(do ~@(map gen1 (api-methods))))

;; ready to generate all 315 methods ;)
;; https://oss.sonatype.org/service/local/repositories/releases/archive/com/slack/api/slack-api-client/1.45.3/slack-api-client-1.45.3-javadoc.jar/!/com/slack/api/methods/MethodsClient.html
(gen-all!)

(comment
  (def public-methods (api-methods))
  (def request-classes
    (->> public-methods
         (mapcat :parameter-types)
         (filter (comp #(str/ends-with? % "Request") name))
         (into #{} (map resolve))))

  (defn builder-methods
    [^Class c]
    (try
      (->> (util/invoke-builder c)
           class
           .getMethods
           seq)
      (catch Exception _ nil)))

  (import '[java.lang.reflect ParameterizedType])

  (def requests-with-nested-builder
    (->> request-classes
         (filter
           (fn [c]
             (some
               (fn [^java.lang.reflect.Method m]
                 (let [g (first (.getGenericParameterTypes m))]
                   (and (instance? ParameterizedType g)
                        (some
                          #(and (instance? Class %)
                                (not (str/starts-with? (.getName ^Class %) "java.")))
                          (.getActualTypeArguments ^ParameterizedType g)))))
               (builder-methods c))))
         (map util/class-name)
         set))

  ;; find all Request classes with nested types (requiring further building)
  (->> (methods j/to-java)
       keys
       (remove #{:default})
       (map (comp util/class-name first))
       (filter #(str/starts-with? % "com.slack.api.methods.request"))
       set
       (set/difference requests-with-nested-builder))


 (gen1 (first public-methods))

  )