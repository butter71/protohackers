(ns butter.pest-control.client
  (:gen-class)
  (:import (java.net Socket))
  (:require [butter.pest-control.conn :refer [read-msg write-bytes send-msg]]
            [butter.pest-control.protocol :refer [make-hello make-dial-authority
                                                  make-create-policy make-delete-policy]]
            [clojure.core.async :refer [thread chan >!! <!! alts!! timeout close!]]
            [clojure.java.io :as io]))

;; pestcontrol.protohackers.com 20547

(def site-channels (atom {}))
(def site-targets (atom {}))

(defn add-site-targets [site targets]
  (let [m (reduce (fn [acc [species pmin pmax]]
                    (assoc acc species [pmin pmax]))
                  {}
                  targets)]
    (swap! site-targets #(assoc % site m))))

(defn authority-client-server [ch site]
  (let [sock (Socket. "pestcontrol.protohackers.com" 20547)
        reader (io/input-stream sock)
        writer (io/output-stream sock)]
    (send-msg writer reader (make-hello "pestcontrol" 1) :hello)
    (if-let [msg (send-msg writer reader (make-dial-authority site) :target-populations)]
      (do (add-site-targets site (:populations msg))
          (thread (loop []
                    (if-let [[rch msg] (<!! ch)]
                      (do (write-bytes writer msg)
                          (if-let [dm (read-msg reader)]
                            (do (>!! rch dm)
                                (recur))
                            (.close sock)))
                      (.close sock)))))
      (.close sock))))

(defn authority-client [site]
  (if-let [ch (@site-channels site)]
    ch
    (let [ch (chan 100)]
      (swap! site-channels #(assoc % site ch))
      (authority-client-server ch site)
      ch)))

(defn request-with-retry [site msg]
  (let [r-ch (chan)]
    (loop [nretries 0]
      (when (>= nretries 5)
        (throw (Exception. "too many timeouts")))
      (let [ch (authority-client site)]
        (>!! ch [r-ch msg])
        (let [timeout-ch (timeout (* (inc nretries) 1000))
              [val port] (alts!! [r-ch timeout-ch])]
          (if (= port timeout-ch)
            (do (close! ch)
                (swap! site-channels #(dissoc % site))
                (recur (inc nretries)))
            (do (close! r-ch)
                val)))))))

(defn create-policy [site species action]
  (->> (make-create-policy species action)
       (request-with-retry site)
       second
       :policy))

(defn delete-policy [site _species policy]
  (request-with-retry site (make-delete-policy policy)))
