(defproject clojure-lox "0.1.0-SNAPSHOT"
  :description "An interpreter for the Lox language written in Clojure."
  :dependencies [[org.clojure/clojure "1.12.2"] 
                 [org.clojure/tools.trace "0.9.0"]]
  :main ^:skip-aot clojure-lox.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
