(ns butter.pest-control.conn
  (:gen-class)
  (:import (java.io BufferedInputStream BufferedOutputStream))
  (:require [butter.pest-control.protocol :refer [decode]]))

(defn read-msg [^BufferedInputStream reader]
  (loop [need 5
         acc []]
    (if (zero? need)
      (let [dm (decode acc)]
        (if (= (first dm) :need)
          (recur (second dm) acc)
          dm))
      (let [b (.read reader)]
        (when-not (neg? b)
          (recur (dec need) (conj acc b)))))))

(defn write-bytes [^BufferedOutputStream writer bs]
  (doto writer
    (.write (byte-array bs) 0 (count bs))
    (.flush)))

(defn send-msg [^BufferedOutputStream writer ^BufferedInputStream reader msg expected]
  (write-bytes writer msg)
  (let [[_r1 r2] (read-msg reader)]
    (when (= (:type r2) expected)
      r2)))
