(defproject prime-time "0.1.0-SNAPSHOT"
  :description "simple prime checking server"
  :dependencies [[org.clojure/clojure "1.11.1"]]
  :repl-options {:init-ns prime-time.core}
  :main prime-time.core
  :aot :all)
