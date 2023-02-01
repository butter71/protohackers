(defproject echo-server "0.1.0-SNAPSHOT"
  :description "simple echo server"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns echo-server.core}
  :main echo-server.core
  :aot :all)
