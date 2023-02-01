(defproject budget-chat "0.1.0-SNAPSHOT"
  :description "simple prime checking server"
  :dependencies [[org.clojure/clojure "1.11.1"]
                 [org.clojure/core.match "1.0.1"]
                 [org.clojure/core.async "1.6.673"]]
  :repl-options {:init-ns budget-chat.core}
  :main budget-chat.core
  :aot :all)
