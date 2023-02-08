(ns butter.insecure-sockets.server
  (:gen-class)
  (:import (java.io BufferedInputStream BufferedOutputStream)
           (java.net Socket ServerSocket))
  (:require [clojure.core.async :refer [thread]]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as s]))

;; cipher

(defn cipher-reverse [b _pos]
  (loop [i 7
         acc 0]
    (if (neg? i)
      (int acc)
      (recur (dec i)
             (+ acc (if (bit-test b (- 7 i))
                      (math/pow 2 i)
                      0))))))

(defn cipher-xor-pos [b pos]
  (bit-xor b (mod pos 256)))

(defn cipher-xor-n [n b _pos]
  (bit-xor b n))

(defn cipher-add-pos [b pos]
  (mod (+ b pos) 256))

(defn cipher-add-n [n b _pos]
  (mod (+ b n) 256))

(defn cipher-sub-pos [b pos]
  (mod (- b pos) 256))

(defn cipher-sub-n [n b _pos]
  (mod (- b n) 256))

;; app layer

(defn toy-priority [s]
  (->> (s/split s #",")
       (map #(let [[n toy] (s/split % #"x " 2)]
               [(parse-long n) toy]))
       (sort-by first >)
       first
       (apply (partial format "%dx %s"))))

;; server

(defn cipherize [cipher b pos]
  (reduce #(%2 %1 pos) b cipher))

(defn valid-cipher? [cipher]
  (not (every? true?
               (for [b (range 0 256)]
                 (= b (cipherize cipher b 15))))))

(defn cipherize-text [cipher text pos]
  (loop [[c & cs] text
         pos pos
         acc []]
    (if (nil? c)
      (conj acc (cipherize cipher 10 pos))
      (recur cs (inc pos) (conj acc (cipherize cipher (int c) pos))))))

(defn write-bytes [^BufferedOutputStream writer bs]
  (doseq [b bs]
    (.write writer b))
  (.flush writer))

(defn get-cipher [^BufferedInputStream reader]
  (loop [b (.read reader)
         state nil
         eacc []
         dacc []]
    (cond (= b -1)
          nil

          (= state :xor)
          (recur (.read reader)
                 nil
                 (conj eacc (partial cipher-xor-n b))
                 (conj dacc (partial cipher-xor-n b)))

          (= state :add)
          (recur (.read reader)
                 nil
                 (conj eacc (partial cipher-add-n b))
                 (conj dacc (partial cipher-sub-n b)))

          (not= state nil)
          nil
          
          (= b 0)
          (if (valid-cipher? eacc)
            [eacc (reverse dacc)]
            nil)

          (= b 1)
          (recur (.read reader) nil (conj eacc cipher-reverse) (conj dacc cipher-reverse))

          (= b 2)
          (recur (.read reader) :xor eacc dacc)

          (= b 3)
          (recur (.read reader) nil (conj eacc cipher-xor-pos) (conj dacc cipher-xor-pos))

          (= b 4)
          (recur (.read reader) :add eacc dacc)

          (= b 5)
          (recur (.read reader) nil (conj eacc cipher-add-pos) (conj dacc cipher-sub-pos))

          :else
          nil)))

(defn priority-server [^Socket sock]
  (with-open [reader (io/input-stream sock)
              writer (io/output-stream sock)]
    (let [[encipher decipher] (get-cipher reader)]
      (if (nil? encipher)
        (do (println "..close (bad cipher)")
            (.close sock)) 
        (loop [ch (.read reader)
               r-pos 0
               w-pos 0
               acc []]
          (if (= -1 ch)
            (do (println "..close")
                (.close sock))
            (let [ch (cipherize decipher ch r-pos)]
              (if (= ch 10)
                (let [req (apply str (map char acc))
                      resp (cipherize-text encipher (toy-priority req) w-pos)]
                  (write-bytes writer resp)
                  (recur (.read reader) (inc r-pos) (+ w-pos (count resp)) []))
                (recur (.read reader) (inc r-pos) w-pos (conj acc ch))))))))))

(defn start-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (priority-server sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (start-server))
