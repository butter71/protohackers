(ns means-to-an-end.core
  (:gen-class)
  (:import (java.io BufferedInputStream BufferedOutputStream)
           (java.net Socket ServerSocket))
  (:require [clojure.core.async :refer [thread]]
            [clojure.java.io :as io]))

(defn <-2c [n]
  (if (> n 2147483647)
    (- n 4294967296)
    n))

(defn ->2c [n]
  (if (neg? n)
    (+ 4294967296 n)
    n))

(defn <-int32 [[a b c d]]
  (+ (* a 16777216)
     (* b 65536)
     (* c 256)
     d))

(defn ->int32 [n]
  (let [d (mod n 256)
        c (mod (quot n 256) 256)
        b (mod (quot n 65536) 256)
        a (mod (quot n 16777216) 256)]
    [a b c d]))

(defn decode-message [[a b c d e f g h i]]
  (let [cmd (char a)
        arg1 (<-2c (<-int32 [b c d e]))
        arg2 (<-2c (<-int32 [f g h i]))]
    [cmd arg1 arg2]))

(defn get-cmd [^BufferedInputStream reader]
  (loop [b (.read reader)
         i 0
         acc []]
    (cond (= -1 b) nil
          (= i 8) (conj acc b)
          :else (recur (.read reader) (inc i) (conj acc b)))))

(defn send-response [^BufferedOutputStream writer resp]
  (loop [[ch & chs] resp]
    (if ch
      (do (.write writer ch)
          (recur chs))
      (.flush writer))))

(defn get-mean [state min max]
  (let [coll (map second (filter #(<= min (first %) max) state))]
    (if (empty? coll)
      0
      (quot (apply + coll) (count coll)))))

(defn means-server [^Socket sock]
  (with-open [reader (io/input-stream sock)
              writer (io/output-stream sock)]
    (loop [cmd (get-cmd reader)
           state {}]
      (if (nil? cmd)
        (.close sock)
        (let [[op arg1 arg2] (decode-message cmd)]
          (println [op arg1 arg2])
          (case op
            \I (recur (get-cmd reader) (assoc state arg1 arg2))
            \Q (let [mean (get-mean state arg1 arg2)]
                 (send-response writer (->int32 (->2c mean)))
                 (recur (get-cmd reader) state))))))))

(defn start-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (means-server sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (start-server))
