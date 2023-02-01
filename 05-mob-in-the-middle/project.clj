(defproject mob-in-the-middle "0.1.0-SNAPSHOT"
  :description "mob in the middle"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns mob-in-the-middle.core}
  :main mob-in-the-middle.core
  :aot :all)
