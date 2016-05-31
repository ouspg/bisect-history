(defproject bisect_history "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/tools.cli "0.3.5"]
                 [org.clojure/data.xml "0.0.8"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [clj-json "0.5.3"]
                 [me.raynes/conch "0.8.0"]]
  :main ^:skip-aot bisect-history.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
