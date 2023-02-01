(ns echo-server.core
  (:gen-class)
  (:import (java.net Socket ServerSocket))
  (:require [clojure.core.async :refer [thread]]
            [clojure.java.io :as io]))

(defn echo-server [^Socket sock]
  (with-open [reader (io/input-stream sock)
              writer (io/output-stream sock)]
    (loop [ch (.read reader)]
      (if (not= -1 ch)
        (do (.write writer ch)
            (.flush writer)
            (recur (.read reader)))
        (do (println "..close")
            (.close sock))))))

(defn start-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (echo-server sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (start-server))
