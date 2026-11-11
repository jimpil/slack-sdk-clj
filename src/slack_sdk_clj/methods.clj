(ns slack-sdk-clj.methods
  (:require [clojure.reflect :as r]
            [clojure.java.data :as j]
            [clojure.java.data.builder :as jb]
            [clojure.string :as str])
  (:import [clojure.lang APersistentMap Reflector]
           [clojure.reflect Method]
           [com.slack.api RequestConfigurator]
           [com.slack.api.methods MethodsClient]
           [com.slack.api.methods.request.files FilesUploadV2Request FilesUploadV2Request$UploadFile]
           [okhttp3 FormBody$Builder MultipartBody$Builder]))

(set! *warn-on-reflection* true)

(defn- from-builder
  "Generic helper for any builder-based class."
  [^Class c props]
  (let [builder (Reflector/invokeStaticMethod c "builder" (object-array 0))]
    (jb/to-java c builder props {})))

(defn- map->obj
  "Single entry-point for converting maps to Java objects.
   Tries the (extensible) `clojure.java.data /to-java`,
   falling back to the Builder pattern which will succeed."
  [t arg]
  (try
    (j/to-java t arg)
    (catch Exception _
      (from-builder t arg))))

;; some Request classes contain fields that need further building
;; teach `clojure.java.data/to-java` how to do it

(defmethod j/to-java [FilesUploadV2Request APersistentMap]
  [_ props]
  ;; the `filesUploadV2` is able to upload multiple files (via :uploadFiles),
  ;; but the param value is expected to be a list of `FilesUploadV2Request$UploadFile`
  (if (contains? props :uploadFiles)
    (->> (partial mapv (partial from-builder FilesUploadV2Request$UploadFile))
         (update props :uploadFiles)
         (from-builder FilesUploadV2Request))
    props))

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
                    (with-meta `(map->obj ~t# ~arg#) {:tag t#})
                   arg#))) ;; plain String probably
              parameter-types)))))

(defmacro ^:private gen-all! []
  `(do ~@(map gen1 (api-methods))))

;; ready to generate all 315 methods ;)
;; https://oss.sonatype.org/service/local/repositories/releases/archive/com/slack/api/slack-api-client/1.45.3/slack-api-client-1.45.3-javadoc.jar/!/com/slack/api/methods/MethodsClient.html
(gen-all!)

(comment
  (def public-methods (api-methods))

  (macroexpand-1 (gen1 (first public-methods)))

  )