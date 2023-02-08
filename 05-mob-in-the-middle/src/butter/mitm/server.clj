(ns butter.mitm.server
  (:gen-class)
  (:import (java.io BufferedReader BufferedWriter)
           (java.net Socket ServerSocket))
  (:require [clojure.string :as s]
            [clojure.core.async :refer [thread chan >!! alts!! close!]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]))

(def tony-boguscoin "7YWHMfk9JZe0LM0g1ZauHuiSxhI")

(defn rewrite-msg [msg]
  (if (= (first msg) \*)
    msg
    (s/join \space (reduce #(if (re-matches #"^7[0-9a-zA-Z]{25,34}$" %2)
                              (conj %1 tony-boguscoin)
                              (conj %1 %2))
                           []
                           (s/split msg #"\s")))))

(defn write-line [^BufferedWriter writer s]
  (doto writer
    (.write s)
    (.newLine)
    (.flush)))

(defn read-name [^BufferedReader reader]
  (loop [c (.read reader)
         acc []]
    (cond (= c -1)
          nil

          (or (<= 65 c 90)
              (<= 97 c 122)
              (<= 48 c 57))
          (recur (.read reader) (conj acc c))

          :else
          (apply str (map char acc)))))

(defn proxy-reader [tag ^BufferedReader reader ch]
  (try (loop [s (.readLine reader)]
         (if (nil? s)
           (close! ch)
           (do (>!! ch [tag s])
               (recur (.readLine reader)))))
       (catch Exception _ (close! ch))))

(defn proxy-connection [^Socket csock ^Socket ssock]
  (with-open [creader (io/reader csock)
              cwriter (io/writer csock)
              sreader (io/reader ssock)
              swriter (io/writer ssock)]
    (write-line cwriter (.readLine sreader))
    (if-let [name (read-name creader)]
      (do (write-line swriter name)
          (let [ch1 (chan 100)
                ch2 (chan 100)
                writer-for {1 swriter 2 cwriter}]
            (thread (proxy-reader 1 creader ch1))
            (thread (proxy-reader 2 sreader ch2))
            (loop []
              (match (alts!! [ch1 ch2])
                [nil _] (do (println "..close")
                            (.close csock)
                            (.close ssock))
                [[ch msg] _] (do (write-line (writer-for ch) (rewrite-msg msg))
                                 (recur))))))
      (do (println "..close")
          (.close csock)
          (.close ssock)))))

(defn proxy-server []
  (with-open [cserver (ServerSocket. 5514)]
    (println "open!")
    (loop [csock (.accept cserver)]
      (println "..accept")
      (thread (proxy-connection csock (Socket. "chat.protohackers.com" 16963)))
      (recur (.accept cserver)))))

(defn -main [& _args]
  (proxy-server))
