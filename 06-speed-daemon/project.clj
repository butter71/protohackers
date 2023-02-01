(defproject speed-daemon "0.1.0-SNAPSHOT"
  :description "speed daemon"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns speed-daemon.core}
  :main speed-daemon.core
  :aot :all)
