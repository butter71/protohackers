(ns line-reversal.core
  (:gen-class)
  (:import (java.net DatagramSocket DatagramPacket))
  (:require [clojure.core.async :refer [thread chan >!! alts!! timeout]]
            [clojure.core.match :refer [match]]
            [clojure.data.priority-map :refer [priority-map]]
            [clojure.string :as s]))

(def packet-size 900)

;; msg decoding

(defn unescape [coll]
  (loop [[c & cs] coll
         escape? false
         acc []]
    (cond (nil? c) (apply str acc)
          escape? (recur cs false (conj acc c))
          (= c \\) (recur cs true acc)
          (= c \/) nil
          :else (recur cs false (conj acc c)))))

(defn ends-in-slash? [str]
  (let [[z y x & _] (reverse str)]
    (and (= z \/)
         (or (= x y \\)
             (not= y \\)))))

(defn decode-message [str]
  (match (s/split str #"/" 5)
    ["" "connect" session ""] {:type :connect :session (parse-long session)}
    ["" "close" session ""] {:type :close :session (parse-long session)}
    ["" "data" session pos (data :guard ends-in-slash?)] (when-let [data (unescape (butlast data))]
                                                           {:type :data
                                                            :session (parse-long session)
                                                            :pos (parse-long pos)
                                                            :data data})
    ["" "ack" session length ""] {:type :ack :session (parse-long session)
                                  :length (parse-long length)}
    :else nil))

(defn packet-data [packet]
  (->> (.getData packet)
       (take-while pos?)
       (map char)
       (apply str)
       s/trim-newline
       decode-message))

;; msg encoding

;; /data/SESSION/POS/DATA/
;; /ack/SESSION/LENGTH/
;; /close/SESSION/

(defn escape [coll]
  (loop [[c & cs] coll
         i 0
         acc []]
    (cond (nil? c) (apply str acc)
          (= c \\) (recur cs (+ i 2) (conj acc \\ \\))
          (= c \/) (recur cs (+ i 2) (conj acc \\ \/))
          :else (recur cs (inc i) (conj acc c)))))

(defn empty-packet []
  (DatagramPacket. (byte-array 1000) 1000))

(defn make-packet [addr port msg]
  (let [packet (empty-packet)]
    (doto packet
      (.setAddress addr)
      (.setPort port)
      (.setData (.getBytes msg) 0 (count msg)))))

(defn make-ack [addr port sid length]
  (make-packet addr port (format "/ack/%d/%d/" sid length)))

(defn make-close [addr port sid]
  (make-packet addr port (format "/close/%d/" sid)))

(defn make-data [addr port sid pos data]
  (loop [[x & xs] (partition-all packet-size (escape data))
         pos pos
         acc []]
    (if (nil? x)
      acc
      (recur xs (+ pos packet-size)
             (conj acc (make-packet addr port (format "/data/%d/%d/%s/" sid pos (apply str x))))))))

(def sessions (atom {}))
(def socket (atom nil))

(defn send-packet [packet]
  (println (format "-> %s %s" (quot (System/currentTimeMillis) 1000) (packet-data packet)))
  (.send @socket packet))

(defn send-packets [packets]
  (doseq [p packets]
    (send-packet p)))

(defn reap-data [rp queue]
  (loop [queue queue
         rp rp
         acc []]
    (let [[[pos data] _] (peek queue)]
      (cond (nil? pos)
            [rp queue acc]

            (> pos rp)
            [rp queue acc]

            :else
            (let [seg (drop (- rp pos) data)]
              (recur (pop queue) (+ rp (count seg)) (concat acc seg)))))))

(defn reverser [coll]
  (loop [[c & cs] coll
         acc []
         res []]
    (cond (nil? c)
          [res acc]

          (= c \newline)
          (recur cs [] (concat res (reverse acc) [\newline]))

          :else
          (recur cs (conj acc c) res))))

(defn lrcp-server [sid addr port ch]
  (loop [rp 0
         rqueue (priority-map)
         wp 0
         wqueue []
         acc []]
    (let [ping (timeout 3000)
          [pmsg pch] (alts!! [ch ping])]
      (if (= pch ping)
        (do (when-not (empty? wqueue)
              (send-packets (make-data addr port sid wp wqueue)))
            (recur rp rqueue wp wqueue acc))
        (do (println (format "<- %s %s" (quot (System/currentTimeMillis) 1000) pmsg))
            (case (:type pmsg)
              :connect
              (do (send-packet (make-ack addr port sid 0))
                  (recur rp rqueue wp wqueue acc))

              :close
              (do (send-packet (make-close addr port sid))
                  (swap! sessions dissoc sid))

              :ack
              (let [length (:length pmsg)]
                (cond (<= length wp)
                      (recur rp rqueue wp wqueue acc)

                      (> length (+ wp (count wqueue)))
                      (do (send-packet (make-close addr port sid))
                          (swap! sessions dissoc sid))

                      :else
                      (let [wqueue (drop (- length wp) wqueue)
                            wp length]
                        (when-not (empty? wqueue)
                          (send-packets (make-data addr port sid wp wqueue)))
                        (recur rp rqueue wp wqueue acc))))

              :data
              (let [pos (:pos pmsg)
                    data (:data pmsg)]
                (if (> pos rp)
                  (do (send-packet (make-ack addr port sid rp))
                      (recur rp (assoc rqueue [pos data] pos) wp wqueue acc))
                  (let [[rp queue reaped] (reap-data rp (assoc rqueue [pos data] pos))
                        [to-send acc] (reverser (concat acc reaped))]
                    (send-packet (make-ack addr port sid rp))
                    (when (and (seq to-send) (empty? wqueue))
                      (send-packets (make-data addr port sid wp to-send)))
                    (recur rp queue wp (concat wqueue to-send) acc))))

              nil))))))

(defn -main [& _args]
  (reset! sessions {})
  (reset! socket (DatagramSocket. 5514))
  (println "open!")
  (loop [packet (empty-packet)]
    (.receive @socket packet)
    (when-let [msg (packet-data packet)]
      (let [mtype (:type msg)
            sid (:session msg)
            sch (get @sessions sid)]
        (if (nil? sch)
          (let [addr (.getAddress packet)
                port (.getPort packet)]
            (case mtype
              :connect (let [sch (chan 100)]
                         (thread (lrcp-server sid addr port sch))
                         (swap! sessions assoc sid sch)
                         (>!! sch msg))
              :data (send-packet (make-close addr port sid))
              :ack nil
              :close (send-packet (make-close addr port sid))))
          (>!! sch msg))))
    (recur (empty-packet))))
