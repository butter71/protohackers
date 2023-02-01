(defproject means-to-an-end "0.1.0-SNAPSHOT"
  :description "simple prime checking server"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns means-to-an-end.core}
  :main means-to-an-end.core
  :aot :all)
