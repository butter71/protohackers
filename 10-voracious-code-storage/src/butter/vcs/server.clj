(ns butter.vcs.server
  (:gen-class)
  (:import (java.net Socket ServerSocket)
           (java.io BufferedInputStream BufferedOutputStream File))
  (:require [butter.vcs.fs :refer [start-fs-server fs-list-dir fs-get-file fs-put-file]]
            [clojure.core.async :refer [thread chan <!! >!!]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;;; inodes

(def inodes (chan 100))

(defn next-inode []
  (<!! inodes))

(defn inode-factory []
  (loop [i 0]
    (>!! inodes i)
    (recur (inc i))))

;;; validation

(def valid-chars
  ;; A-Z, a-z, _./-, 0-9
  (set (map char (concat (range 65 91) [95] (range 97 123) (range 45 58)))))

(defn valid-filename? [s]
  (and (= (first s) \/)
       (not= (last s) \/)
       (every? valid-chars s)))

(def ascii-chars
  (set (concat (range 32 127) [9 10])))

(defn not-ascii? [b]
  (not (ascii-chars b)))

(defn valid-dir? [s]
  (and (= (first s) \/)
       (every? valid-chars s)))

(def usage-text
  {:HELP "OK usage: HELP|GET|PUT|LIST"
   :LIST "ERR usage: LIST dir"
   :GET "ERR usage: GET file [revision]"
   :PUT "ERR usage: PUT file length newline data"})

(def error-text
  {:ILLEGAL-DIR "ERR illegal dir name"
   :ILLEGAL-FN "ERR illegal file name"
   :ILLEGAL-METHOD "ERR illegal method: "
   :NO-SUCH-FILE "ERR no such file"
   :NO-SUCH-REV "ERR no such revision"
   :TEXT-FILES-ONLY "ERR text files only"})

;;; i/o

(defn read-to-file [^BufferedInputStream reader n fn]
  (if (= n 0)
    []
    (with-open [out (io/output-stream (io/file fn))]
      (let [crc32 (java.util.zip.CRC32.)]
        (loop [b (.read reader)
               i 1]
          (cond (= b -1) nil
                (not-ascii? b) :bin
                (= i n) (do
                          (.write out b)
                          (.update crc32 b)
                          (.getValue crc32))
                :else (do (.write out b)
                          (.update crc32 b)
                          (recur (.read reader) (inc i)))))))))

(defn write-from-file [^BufferedOutputStream writer fn]
  (with-open [in (io/input-stream (io/file fn))]
    (io/copy in writer)
    (.flush writer)))

(defn read-bytes [^BufferedInputStream reader n]
  (if (= n 0)
    []
    (loop [b (.read reader)
           i 1
           acc []]
      (cond (= b -1) nil
            (= i n) (conj acc b)
            :else (recur (.read reader) (inc i) (conj acc b))))))

(defn read-cmd [^BufferedInputStream reader]
  (loop [b (.read reader)
         acc []]
    (cond (= b -1) nil
          (= b 10) (apply str (map char acc))
          :else (recur (.read reader) (conj acc b)))))

(defn write-bytes [^BufferedOutputStream writer bs]
  (.write writer (byte-array bs) 0 (count bs))
  (.flush writer))

(defn write [^BufferedOutputStream writer ^String s]
  (doto writer
    (.write (.getBytes s) 0 (count s))
    (.write 10)
    (.flush )))

;;; parsing 

(defn parse-rev [s]
  (if (nil? s)
    :head
    (if-let [[_ rev] (re-matches #"^r?(\d+).*" s)]
      (parse-long rev)
      0)))

(defn parse-length [s]
  (if-let [[_ length] (re-matches #"^(\d+).*" s)]
    (parse-long length)
    0))

(defn parse-request [s]
  (let [[cmd & args] (str/split s #"\s+")
        cmd (str/upper-case cmd)]
    (case cmd
      "HELP" [:USAGE :HELP]

      "LIST" (if (= 1 (count args))
               (let [dir (first args)]
                 (if (valid-dir? dir)
                   [:LIST dir]
                   [:ERROR :ILLEGAL-DIR false]))
               [:USAGE :LIST])

      "GET" (if (#{1 2} (count args))
              (let [[fname rev] args]
                (if (valid-filename? fname)
                  [:GET fname (parse-rev rev)]
                  [:ERROR :ILLEGAL-FN false]))
              [:USAGE :GET])

      "PUT" (if (= 2 (count args))
              (let [fname (first args)
                    length (parse-length (second args))]
                (if (valid-filename? fname)
                  [:PUT fname length]
                  [:ERROR :ILLEGAL-FN false]))
              [:USAGE :PUT])

      [:CLOSE :ILLEGAL-METHOD cmd])))

;;; servers

(defn vcs-conn [^Socket sock]
  (with-open [reader (io/input-stream sock)
              writer (io/output-stream sock)]
    (let [ch (chan 100)]
      (loop [show-prompt? true]
        (when show-prompt?
          (write writer "READY"))
        (if-let [req (read-cmd reader)]
          (match (parse-request req)
            [:USAGE cmd]
            (do
              (write writer (usage-text cmd))
              (recur true))

            [:LIST dir]
            (let [entries (fs-list-dir ch dir)]
              (write writer (format "OK %d" (count entries)))
              (doseq [[name rev] (sort-by first entries)]
                (if (zero? rev)
                  (write writer (format "%s/ DIR" name))
                  (write writer (format "%s r%d" name rev))))
              (recur true))

            [:GET fn rev]
            (let [resp (fs-get-file ch fn rev)]
              (if (= :ERROR (first resp))
                (do
                  (write writer (error-text (second resp)))
                  (recur true))
                (let [[_rev {:keys [length inode]}] resp]
                  (write writer (format "OK %d" length))
                  (write-from-file writer (format "/tmp/vcs/%s" inode))
                  (recur true))))
            
            [:PUT fn length]
            (let [inode (next-inode)
                  fname (format "/tmp/vcs/%s" inode)
                  cksum (read-to-file reader length fname)]
              (case cksum
                nil (do
                      (println "..close (eof)")
                      (.close sock))

                :bin
                (do
                  (write writer (error-text :TEXT-FILES-ONLY))
                  (recur true))

                (let [rev (fs-put-file ch fn length inode cksum)]
                  (write writer (format "OK r%d" rev))
                  (recur true))))
            
            [:ERROR msg prompt?]
            (do
              (write writer (error-text msg))
              (recur prompt?))

            [:CLOSE msg cmd]
            (do
              (write writer (format "%s%s" (error-text msg) cmd))
              (println "..close (err)")
              (.close sock)))
          (do
            (println "..close (eof)")
            (.close sock)))))))

(defn vcs-server []
  (let [server (ServerSocket. 5514)]
    (println "open!")
    (loop [sock (.accept server)]
      (println "..accept")
      (thread (vcs-conn sock))
      (recur (.accept server)))))

(defn -main [& _args]
  (.mkdir (File. "/tmp/vcs/"))
  (thread (inode-factory))
  (start-fs-server)
  (vcs-server))
