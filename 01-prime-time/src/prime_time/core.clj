(ns prime-time.core
  (:gen-class)
  (:import (java.net Socket ServerSocket)
           (java.io BufferedWriter))
  (:require [clojure.core.async :refer [thread]]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.math :as math]))

(def replies
  {true "{\"method\":\"isPrime\",\"prime\":true}\n"
   false "{\"method\":\"isPrime\",\"prime\":false}\n"
   :malformed "malformed\n"})

(defn prime? [^clojure.lang.BigInt n]
  (letfn [(divides? [m n] (zero? (rem m n)))]
    (and (< 1 n) (not-any? #(divides? n %) (range 2 (inc (bigint (math/sqrt n))))))))

(defn send-response [^BufferedWriter writer ^String resp]
  (doto writer
    (.write resp 0 (count resp))
    (.flush)))

(defn parse-request [str]
  (try (let [req (json/read-str str)
             method (get req "method")
             number (get req "number")]
         (cond (not= method "isPrime") nil
               (int? number) (bigint number)
               (= clojure.lang.BigInt (type number)) number
               (float? number) 0N
               :else nil))
       (catch Exception _e nil)))

(defn prime-server [^Socket sock]
  (with-open [reader (io/reader sock)
              writer (io/writer sock)]
    (loop [req (.readLine reader)]
      (if (nil? req)
        (.close sock)
        (if-let [n (parse-request req)]
          (do (send-response writer (get replies (prime? n)))
              (recur (.readLine reader)))
          (do (send-response writer (get replies :malformed))
              (println "malformed: " req)
              (.close sock)))))))

(defn start-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (prime-server sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (start-server))
