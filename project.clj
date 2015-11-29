(defproject mwdict "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.fusesource.jansi/jansi "1.11"]]
  :plugins [[lein-bin "0.3.5"]]
  :main ^:skip-aot mwdict.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :bin {:name "mwdict"})
