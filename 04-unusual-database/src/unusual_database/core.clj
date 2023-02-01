(ns unusual-database.core
  (:gen-class)
  (:import (java.net DatagramSocket DatagramPacket))
  (:require [clojure.string :as s]
            [clojure.core.match :refer [match]]))

(def db (atom {}))

(defn empty-packet []
  (DatagramPacket. (byte-array 1000) 1000))

(defn packet-data [^DatagramPacket packet]
  (apply str (map char (take-while pos? (.getData packet)))))

(defn set-packet-data [^DatagramPacket packet msg]
  (doto packet
    (.setData (.getBytes msg) 0 (count msg))))

(defn parse-request [body]
  (match (s/split body #"=" 2)
    ["version" _] nil
    [k v] [:insert k v]
    [k] [:request k]))

(defn update-db [req]
  (println ":" req)
  (match req
    nil nil
    [:insert k v] (do (swap! db #(assoc % k v))
                      nil)
    [:request k] (let [v (get @db k "")]
                   (s/join "=" [k v]))))

(defn -main [& _args]
  (let [server (DatagramSocket. 5514)]
    (reset! db {"version" "MonKey Store v1.0"})
    (println "open!")
    (loop [packet (empty-packet)]
      (.receive server packet)
      (println "<-" (.getHostAddress (.getAddress packet)) (.getPort packet) (packet-data packet))
      (when-let [reply (update-db (parse-request (packet-data packet)))]
        (println "->" reply)
        (.send server (set-packet-data packet reply)))
      (recur (empty-packet)))))
