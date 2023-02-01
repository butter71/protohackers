(defproject voracious-code-storage "0.1.0-SNAPSHOT"
  :description "voracious code storage"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns voracious-code-storage.core}
  :main voracious-code-storage.core)
