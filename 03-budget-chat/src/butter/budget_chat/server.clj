(ns butter.budget-chat.server
  (:gen-class)
  (:import (java.io BufferedReader BufferedWriter)
           (java.net Socket ServerSocket))
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.core.async :refer [thread chan >!! alts!! close!]]
            [clojure.core.match :refer [match]]))

(def users (atom {}))

(defn valid-name [name]
  (and (pos? (count name))
       (every? #(let [c (int %)]
                  (or (<= 65 c 90)
                      (<= 97 c 122)
                      (<= 48 c 57)))
               name)))

(defn write-line [^BufferedWriter writer s]
  (doto writer
    (.write s)
    (.newLine)
    (.flush)))

(defn prompt-for-user-name [^BufferedReader reader ^BufferedWriter writer]
  (write-line writer "Welcome to budgetchat! What shall I call you?")
  (let [name (.readLine reader)]
    (if (valid-name name)
      name
      nil)))

(defn user-reader [^BufferedReader reader ch]
  (loop [s (.readLine reader)]
    (if (nil? s)
      (close! ch)
      (do (>!! ch [:input s])
          (recur (.readLine reader))))))

(defn user-broadcast [user msg body]
  (doseq [[name ch] @users]
    (when-not (= user name)
      (println "sending" [msg body])
      (>!! ch [msg body]))))

(defn user [^Socket sock]
  (with-open [reader (io/reader sock)
              writer (io/writer sock)]
    (if-let [name (prompt-for-user-name reader writer)]
      (let [ch (chan 100)]
        (swap! users #(assoc % name ch))
        (user-broadcast name :join name)
        (write-line writer (format "* The room contains: %s" (s/join ", " (remove #(= name %) (keys @users)))))
        (thread (user-reader reader ch))
        (loop []
          (match (alts!! [ch])
            [nil _] (do (println "..close " name)
                        (swap! users #(dissoc % name))
                        (user-broadcast name :left name)
                        (.close sock))
            [[:input b] _] (do (user-broadcast name :msg (format "[%s] %s" name b))
                               (recur))
            [[:msg b] _] (do (write-line writer b)
                             (recur))
            [[:left u] _] (do (write-line writer (format "* %s has left the room" u))
                              (recur))
            [[:join u] _] (do (write-line writer (format "* %s has joined the room" u))
                              (recur)))))
      (do (write-line writer "invalid name")
          (println "..close (invalid name)")
          (.close sock)))))

(defn chat-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept" (keys @users))
      (thread (user sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (reset! users {})
  (chat-server))
