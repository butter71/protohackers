(defproject job-centre "0.1.0-SNAPSHOT"
  :description "job centre"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]
                 [org.clojure/data.priority-map "1.1.0"]]
  :repl-options {:init-ns job-centre.core}
  :main job-centre.core)
