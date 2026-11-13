(defproject slack-sdk-clj "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.12.3"]
                 [org.clojure/java.data "1.3.113"]
                 [com.slack.api/slack-api-client "1.45.4"]
                 [metosin/jsonista "1.0.2"]]
  :repl-options {:init-ns slack-sdk-clj.core})
