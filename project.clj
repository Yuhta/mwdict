(defproject mwdict "0.1.0-SNAPSHOT"
  :description "Command line interface to Merriam-Webster dictionary"
  :url "https://github.com/Yuhta/mwdict"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.fusesource.jansi/jansi "1.11"]]
  :plugins [[lein-bin "0.3.5"]]
  :main ^:skip-aot mwdict.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}}
  :bin {:name "mwdict"})
