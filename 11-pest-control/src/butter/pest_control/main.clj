(ns butter.pest-control.main
  (:gen-class)
  (:require [butter.pest-control.client :refer [site-channels site-targets]]
            [butter.pest-control.policy :refer [policy-server policy-channels]]
            [butter.pest-control.server :refer [pc-server]]
            [clojure.core.async :refer [thread]]))

(defn -main [& _args]
  (reset! site-targets {})
  (reset! site-channels {})
  (doseq [ch (vals policy-channels)]
    (thread (policy-server ch)))
  (pc-server))
