(ns butter.job-centre.server
  (:gen-class)
  (:import (java.io BufferedReader BufferedWriter)
           (java.net Socket ServerSocket))
  (:require [clojure.core.async :refer [thread chan <!! >!! close!]]
            [clojure.core.match :refer [match]]
            [clojure.data.json :as json]
            [clojure.data.priority-map :refer [priority-map-by]]
            [clojure.java.io :as io]))

(def q-ch (chan 10000))

;; responses
(defn make-error [msg]
  (json/write-str {"status" "error" "error" msg}))

(defn make-status-ok
  ([]
   (json/write-str {"status" "ok"}))
  ([id]
   (json/write-str {"status" "ok" "id" id}))
  ([q id job pri]
   (json/write-str {"status" "ok" "id" id
                    "job" job "pri" pri
                    "queue" q})))

(defn make-status-nojob []
  (json/write-str {"status" "no-job"}))

;; queue server

(defn pick-from-queues [queues qs]
  (->> qs
       (map (comp first queues))
       (remove nil?)
       (sort-by second >)
       first
       first))

(defn remove-watcher [watchers client]
  (into {} (map (fn [[k v]] (vector k (disj v client))) watchers)))

(defn add-watcher [watchers client qs]
  (reduce #(update %1 %2 (fnil conj #{}) client) watchers qs))

(defn q-server []
  (loop [next-id 0
         queues {}
         jobs {}
         watchers {}]
    (println "q:" (count queues) "jobs:" (count jobs) "watchers:" (count watchers))
    (let [msg (<!! q-ch)]
      (match msg
        [sch :put q job pri]
        (if-let [client (first (get watchers q))]
          (let [id next-id]
            (>!! client [:job q id job pri])
            (>!! sch [:ok id])
            (recur (inc next-id) queues (assoc jobs id [q job pri]) (remove-watcher watchers client)))
          (let [id next-id]
            (>!! sch [:ok id])
            (recur (inc next-id)
                   (update queues q (fnil assoc (priority-map-by >)) id pri)
                   (assoc jobs id [q job pri])
                   watchers)))

        [sch :get qs wait?]
        (if-let [id (pick-from-queues queues qs)]
          (let [[q job pri] (get jobs id)]
            (>!! sch [:job q id job pri])
            (recur next-id (update queues q pop) jobs watchers))
          (if wait?
            (recur next-id queues jobs (add-watcher watchers sch qs))
            (do
              (>!! sch :no-job)
              (recur next-id queues jobs watchers))))
        
        [sch :abort id]
        (if-let [[q job pri] (get jobs id)]
          (if-let [client (first (get watchers q))]
            (do
              (>!! sch :ok)
              (>!! client [:job q id job pri])
              (recur next-id queues jobs (remove-watcher watchers client)))
            (do
              (>!! sch :ok)
              (recur next-id
                     (update queues q (fnil assoc (priority-map-by >)) id pri)
                     jobs
                     watchers)))
          (do
            (>!! sch :no-job)
            (recur next-id queues jobs watchers)))

        [sch :delete id]
        (if-let [[q _job _pri] (get jobs id)]
          (do
            (>!! sch :ok)
            (recur next-id (update queues q dissoc id)
                   (dissoc jobs id)
                   watchers))
          (do
            (>!! sch :no-job)
            (recur next-id queues jobs watchers)))
        
        nil
        (println "q-server stopped." queues jobs watchers)))))

;; client connections

(defn user-reader [^BufferedReader reader ch]
  (loop [s (.readLine reader)]
    (if (nil? s)
      (close! ch)
      (let [msg (try (json/read-str s)
                     (catch Exception _e :error))]
        (>!! ch msg)
        (recur (.readLine reader))))))

(defn write-line [^BufferedWriter writer ^String s]
  (doto writer
    (.write s 0 (count s))
    (.newLine)
    (.flush)))

(defn job-conn [^Socket sock]
  (with-open [reader (io/reader sock)
              writer (io/writer sock)]
    (let [ch (chan 100)]
      (thread (user-reader reader ch))
      (loop [ids #{}]
        (let [msg (<!! ch)]
          (match msg
            nil
            (do
              (println "..close (eof)")
              (doseq [id ids]
                (>!! q-ch [ch :abort id]))
              (.close sock))

            :error
            (do
              (write-line writer (make-error "invalid JSON"))
              (recur ids))

            :ok
            (do
              (write-line writer (make-status-ok))
              (recur ids))

            [:ok id]
            (do
              (write-line writer (make-status-ok id))
              (recur ids))

            :no-job
            (do
              (write-line writer (make-status-nojob))
              (recur ids))

            [:job q id job pri]
            (do
              (write-line writer (make-status-ok q id job pri))
              (recur (conj ids id)))

            {"request" "put" "queue" q "job" job "pri" pri}
            (do
              (>!! q-ch [ch :put q job pri])
              (recur ids))

            {"request" "get" "queues" qs}
            (do
              (>!! q-ch [ch :get qs (get msg "wait" false)])
              (recur ids))

            {"request" "abort" "id" id}
            (if (ids id)
              (do
                (>!! q-ch [ch :abort id])
                (recur (disj ids id)))
              (do
                (write-line writer (make-error "not owner"))
                (recur ids)))

            {"request" "delete" "id" id}
            (do
              (>!! q-ch [ch :delete id])
              (recur ids))

            :else
            (do
              (write-line writer (make-error "bad request"))
              (recur ids))))))))

(defn job-server []
  (with-open [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (job-conn sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (thread (q-server))
  (job-server))


