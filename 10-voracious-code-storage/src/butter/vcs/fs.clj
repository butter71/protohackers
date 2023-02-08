(ns butter.vcs.fs
  (:gen-class)
  (:require [clojure.core.async :refer [thread chan >!! <!!]]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]))

(def fs-chan (atom (chan 100)))

(defn fname->keypath [s]
  (->> (str/split s #"/")
       rest
       (interpose :children)
       vec))

(defn get-dir [fs s]
  (if (= s "/")
    (map (fn [[k v]] (vector k (count (:revs v)))) fs)
    (if-let [dir (get-in fs (conj (fname->keypath s) :children))]
      (map (fn [[k v]] (vector k (count (:revs v)))) dir)
      [])))

(defn get-file
  ([fs s]
   (if-let [file (get-in fs (conj (fname->keypath s) :revs))]
     (let [current-rev (count file)
           latest-version (get file current-rev)]
       [current-rev latest-version])
     [:ERROR :NO-SUCH-FILE]))
  ([fs s rev]
   (if-let [file (get-in fs (conj (fname->keypath s) :revs))]
     (if-let [rev-version (get file rev)]
       [rev rev-version]
       [:ERROR :NO-SUCH-REV])
     [:ERROR :NO-SUCH-FILE])))

(defn add-file [fs s length inode cksum]
  (let [[current-rev {cr-cksum :cksum}] (get-file fs s)]
    (cond (= current-rev :ERROR)
          [(assoc-in fs (concat (fname->keypath s) [:revs 1])
                     {:length length
                      :inode inode
                      :cksum cksum})
           1]

          (= cksum cr-cksum)
          [fs current-rev]

          :else 
          (let [new-rev (inc current-rev)]
            [(assoc-in fs (concat (fname->keypath s) [:revs new-rev])
                       {:length length
                        :inode inode
                        :cksum cksum})
             new-rev]))))

(defn fs-server []
  (loop [fs {}]
    (let [event (<!! @fs-chan)]
      (println (second event))
      (match event
        [ch [:LIST dir]]
        (do
          (>!! ch (get-dir fs dir))
          (recur fs))

        [ch [:GET file]]
        (do
          (>!! ch (get-file fs file))
          (recur fs))

        [ch [:GET file rev]]
        (do
          (>!! ch (get-file fs file rev))
          (recur fs))

        [ch [:PUT file length inode cksum]]
        (let [[fs rev] (add-file fs file length inode cksum)]
          (>!! ch rev)
          (recur fs))))))

(defn start-fs-server []
  (reset! fs-chan (chan 100))
  (thread (fs-server)))

(defn fs-list-dir [ch dir]
  (>!! @fs-chan [ch [:LIST dir]])
  (<!! ch))

(defn fs-get-file
  ([ch file]
   (>!! @fs-chan [ch [:GET file]])
   (<!! ch))
  ([ch file rev]
   (if (= rev :head)
     (fs-get-file ch file)
     (do (>!! @fs-chan [ch [:GET file rev]])
         (<!! ch)))))

(defn fs-put-file [ch file length inode cksum]
  (>!! @fs-chan [ch [:PUT file length inode cksum]])
  (<!! ch))

;;;
;;;
;;;

(comment
  {"a"
   {:children
    {"b"
     {:revs
      {1 {:length 5, :inode 4, :cksum 555}}
      :children
      {"c"
       {:revs
        {1 {:length 5, :inode 0, :cksum 1234},
         2 {:length 5, :inode 1, :cksum 431}}},
       "d"
       {:revs
        {1 {:length 5, :inode 2, :cksum 314},
         2 {:length 5, :inode 3, :cksum 56314}}}}}
     "e"
     {:children
      {"f"
       {:revs
        {1 {:length 5 :inode 5 :cksum 5151}}}}}}}}
  )
