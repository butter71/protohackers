(ns butter.pest-control.server
  (:gen-class)
  (:import (java.net Socket ServerSocket))
  (:require [butter.pest-control.conn :refer [read-msg send-msg write-bytes]]
            [butter.pest-control.policy :refer [site-to-policy-channel]]
            [butter.pest-control.protocol :refer [make-error make-hello]]
            [clojure.core.async :refer [thread >!!]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]))

(defn pc-conn [^Socket sock]
  (with-open [reader (io/input-stream sock)
              writer (io/output-stream sock)]
    (if (send-msg writer reader (make-hello "pestcontrol" 1) :hello)
      (loop []
        (when-let [[_ dm :as resp] (read-msg reader)]
          (match resp
            [:msg {:type :hello}]
            (recur)

            [:msg {:type :site-visit :site site}]
            (do (>!! (site-to-policy-channel site) dm)
                (recur))

            [:msg _]
            (do (write-bytes writer (make-error "unexpected msg"))
                (recur))

            [:error m]
            (write-bytes writer (make-error m)))))
      (write-bytes writer (make-error "you don't even say hello??")))))

(defn pc-server []
  (let [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (thread (pc-conn sock))
      (recur (.accept server)))))
