(defproject unusual-database "0.1.0-SNAPSHOT"
  :description "unusual database"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]]
  :repl-options {:init-ns unusual-database.core}
  :main unusual-database.core
  :aot :all)
