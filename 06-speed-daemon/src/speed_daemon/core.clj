(ns speed-daemon.core
  (:gen-class)
  (:import (java.net Socket ServerSocket)
           (java.io BufferedInputStream BufferedOutputStream IOException))
  (:require [clojure.core.async :refer [thread chan >!! <!! alts!! close!]]
            [clojure.core.match :refer [match]]
            [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.set :as set]))

;; datatypes

(def dispatches (atom []))

(defn <-u16 [[b1 b2]]
  (+ (* 256 b1) b2))

(defn ->u16 [n]
  [(quot n 256) (rem n 256)])

(defn <-u32 [[b1 b2 b3 b4]]
  (+ (* 256 256 256 b1) (* 256 256 b2) (* 256 b3) b4))

(defn ->u32 [n]
  (loop [n n
         i 3
         acc []]
    (if (zero? i)
      (conj acc (int n))
      (recur (rem n (math/pow 256.0 i))
             (dec i)
             (conj acc (int (quot n (math/pow 256.0 i))))))))

(defn <-str [bs]
  (apply str (map char bs)))

(defn ->str [s]
  (reduce #(conj %1 (int %2)) [(count s)] s))

;; server msgs

(defn make-error [msg]
  (cons 0x10 (->str msg)))

(defn make-ticket [plate road p1 p2 speed]
  (let [[[m1 t1] [m2 t2]] (sort-by second [p1 p2])]
    (flatten [0x21
              (->str plate)
              (->u16 road)
              (->u16 m1)
              (->u32 t1)
              (->u16 m2)
              (->u32 t2)
              (->u16 (* 100 speed))])))

(defn make-heartbeat []
  [0x41])

;; client msgs

(defn plate [[l & bs]]
  {:type :plate
   :plate (<-str (take l bs))
   :timestamp (<-u32 (drop l bs))})

(defn want-heartbeat [bs]
  {:type :want-heartbeat
   :interval (<-u32 bs)})

(defn i-am-camera [bs]
  (let [[a b c] (partition 2 bs)]
    {:type :i-am-camera
     :road (<-u16 a)
     :mile (<-u16 b)
     :limit (<-u16 c)}))

(defn i-am-dispatcher [bs]
  {:type :i-am-dispatcher
   :roads (map <-u16 (partition 2 (rest bs)))})

(defn decode-bytes [[type & bs]]
  (let [len (count bs)]
    (cond (nil? (#{0x20 0x40 0x80 0x81} type))
          :error

          (< len 2)
          :incomplete

          (and (= type 0x20)
               (= len (+ 1 (first bs) 4)))
          (plate bs)

          (and (= type 0x40)
               (= len 4))
          (want-heartbeat bs)

          (and (= type 0x80)
               (= len 6))
          (i-am-camera bs)

          (and (= type 0x81)
               (= len (+ 1 (* 2 (first bs)))))
          (i-am-dispatcher bs)

          :else
          :incomplete)))

;; ticketing logic

(def ticket-ch (atom nil))

(defn day [ts]
  (quot ts 86400))

(defn avg-speed [[m1 t1] [m2 t2]]
  (math/round (abs (* 3600 (/ (- m2 m1) (- t2 t1))))))

;; 1  check if plate is on previously ticketed day
;; 2a pull all combinations that are > limit
;;   -   pick nearest (in time)
;;   -   send ticket
;;   -   update ticketed days
;;   -   remove previous tickets that are within day range
;; 2b add plate to list

(defn scrub-logbook [entries days-covered]
  (remove (fn [e] (contains? days-covered (day (second e)))) entries))

(defn ticket-server []
  (reset! ticket-ch (chan 100))
  (loop [logbook {}
         ticketed {}]
    (let [[{:keys [road mile limit]} plate ts] (<!! @ticket-ch)]
      (if (contains? (get ticketed plate) (day ts))
        (recur logbook ticketed)
        (let [poss (filter #(< limit (avg-speed [mile ts] %)) (get-in logbook [plate road]))]
          (if (empty? poss)
            (recur (update-in logbook [plate road] (fnil conj #{}) [mile ts]) ticketed)
            (let [[mile2 ts2] (first (sort (fn [[_ t1] [_ t2]] (< (abs (- ts t1)) (abs (- ts t2))))
                                           poss))
                  days-covered (set (range (day (min ts ts2)) (inc (day (max ts ts2)))))
                  speed (avg-speed [mile ts] [mile2 ts2])
                  ticketed (update ticketed plate set/union days-covered)
                  logbook (update-in logbook [plate road] #(scrub-logbook % (get ticketed plate)))]
              (>!! (@dispatches road) (make-ticket plate road [mile ts] [mile2 ts2] speed))
              (recur logbook ticketed))))))))

;; server protocol

(defn heartbeat [ch interval]
  (when (< 0 interval)
    (Thread/sleep interval)
    (when-let [_ (>!! ch (make-heartbeat))]
      (recur ch interval))))

(defn write-bytes [^BufferedOutputStream writer bs]
  (doseq [b bs]
    (.write writer b))
  (.flush writer))

(defn close [^Socket sock ^BufferedOutputStream writer msg]
  (if msg
    (do (write-bytes writer (make-error msg))
        (println "..close (error)"))
    (println "..close (eof)"))
  (.close sock))

(defn speed-reader [ch ^Socket sock]
  (let [reader (io/input-stream sock)]
    (try
      (loop [b (.read reader)
             acc []]
        (if (= b -1)
          (close! ch)
          (let [acc (conj acc b)
                msg (decode-bytes acc)]
            (if (= msg :incomplete)
              (recur (.read reader) acc)
              (do (>!! ch msg)
                  (recur (.read reader) []))))))
      (catch IOException _ (close! ch)))))

(defn speed-server [^Socket sock]
  (let [writer (io/output-stream sock)
        rch (chan 100)]
    (thread (speed-reader rch sock))
    (loop [state {}
           chans [rch]]
      (let [[msg _] (alts!! chans)]
        #_(println msg)
        (match msg
          nil
          (close sock writer nil)

          :error
          (close sock writer (make-error "nope"))

          {:type :i-am-camera}
          (if (state :role)
            (close sock writer (make-error "can't switch roles"))
            (recur (assoc state
                          :role :camera
                          :camera {:road (get msg :road)
                                   :mile (get msg :mile)
                                   :limit (get msg :limit)})
                   chans))

          {:type :i-am-dispatcher}
          (if (state :role)
            (close sock writer (make-error "can't switch roles"))
            (let [chans (reduce #(conj %1 (@dispatches %2)) chans (get msg :roads))]
              (recur (assoc state :role :dispatcher) chans)))

          {:type :plate}
          (if (not= (state :role) :camera)
            (close sock writer (make-error "not a camera"))
            (do (>!! @ticket-ch [(state :camera) (get msg :plate) (get msg :timestamp)])
                (recur state chans)))

          {:type :want-heartbeat}
          (if (state :heartbeat)
            (close sock writer (make-error "heartbeat already set"))
            (do 
              (thread (heartbeat rch (* (get msg :interval) 100)))
              (recur (assoc state :heartbeat (get msg :interval)) chans)))

          ([& _] :seq)
          (do #_(println (state :role) "writing " msg)
              (write-bytes writer msg)
              (recur state chans))

          :else
          (recur state chans))))))

(defn start-server []
  (thread (ticket-server))
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (speed-server sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (reset! ticket-ch nil)
  (reset! dispatches (into [] (repeatedly 65536 (partial chan 100))))
  (start-server))
