(defproject insecure-sockets "0.1.0-SNAPSHOT"
  :description "insecure sockets"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns insecure-sockets.core}
  :main insecure-sockets.core)
