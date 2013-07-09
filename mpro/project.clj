(defproject mpro "0.1.0-SNAPSHOT"
  :description "App for Mama!"
  :url "http://example.com/FIXME"
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [org.clojure/java.jdbc "0.3.0-alpha4"]
                 [mysql/mysql-connector-java "5.1.6"]
                 [korma "0.3.0-RC5"]
                 [ring "1.1.8"]
                 [jayq "2.4.0"]
                 [endophile "0.1.0"]
                 [hiccup "1.0.3"]]
  :plugins [[lein-cljsbuild "0.3.0"]
            [lein-ring "0.8.3"]]
  :hooks [leiningen.cljsbuild]
  :source-paths ["src/clj"]
  :cljsbuild { 
    :builds {
      :main {
        :source-paths ["src/cljs"]
        :compiler {:output-to "resources/public/js/cljs.js"
                   :optimizations :simple
                   :pretty-print true}
        :jar true}}}
  :main mpro.server
  :ring {:handler dooperdoo.mpro/app})

