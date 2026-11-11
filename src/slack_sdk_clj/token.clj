(ns slack-sdk-clj.token
  (:require [clojure.string :as str]))

(defn bot?  [token] (str/starts-with? token "xoxb-"))
(defn user? [token] (str/starts-with? token "xoxp-"))

(defn find-in-env
  "Returns the first env-var starting with 'SLACK' or 'slack',
   whose value looks like a bot's, or a user's token (first 5 character check)."
  []
  (some
    (fn [[^String k v]]
      (when (and (.equalsIgnoreCase "slack" (subs k 0 (min 5 (count k))))
                 (or (bot? v)
                     (user? v)))
        v))
    (System/getenv)))