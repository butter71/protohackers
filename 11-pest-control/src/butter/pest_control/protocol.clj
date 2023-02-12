(ns butter.pest-control.protocol
  (:gen-class)
  (:require [clojure.math :as math]
            [clojure.string :as str]))

;; wire types

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

(defn ->str [str]
  (reduce conj (->u32 (count str)) (map int str)))

(defn add-checksum [bs]
  (conj (apply vector bs) (- 256 (mod (apply + bs) 256))))

(defn valid-checksum? [bs]
  (zero? (mod (apply + bs) 256)))

(defn pprint [bs]
  (str/join " " (map #(format "%02x" %) bs)))

;; protocol

(defn make-hello [proto ver]
  (let [enc-proto (->str proto)
        size (+ 1 4 (count enc-proto) 4 1)
        msg (flatten [0x50
                      (->u32 size)
                      enc-proto
                      (->u32 ver)])]
    (add-checksum msg)))

(defn make-error [err]
  (let [enc-err (->str err)
        size (+ 1 4 (count enc-err) 1)
        msg (flatten [0x51
                      (->u32 size)
                      enc-err])]
    (add-checksum msg)))

(defn make-ok []
  [0x52 0x00 0x00 0x00 0x06 0xa8])

(defn make-dial-authority [site]
  (let [size (+ 1 4 4 1)
        msg (flatten [0x53
                      (->u32 size)
                      (->u32 site)])]
    (add-checksum msg)))

(defn make-create-policy [species action]
  (let [enc-species (->str species)
        size (+ 1 4 (count enc-species) 1 1)
        msg (flatten [0x55
                      (->u32 size)
                      enc-species
                      (case action
                        :cull 0x90
                        :conserve 0xa0)])]
    (add-checksum msg)))

(defn make-delete-policy [policy]
  (let [size (+ 1 4 4 1)
        msg (flatten [0x56
                      (->u32 size)
                      (->u32 policy)])]
    (add-checksum msg)))

(defn decode-type [t bs]
  (if (< (count bs) 4)
    nil
    (case t
      :u32 [(<-u32 (take 4 bs)) (drop 4 bs)]
      :str (let [size (<-u32 (take 4 bs))
                 bs (drop 4 bs)]
             (if (< (count bs) size)
               nil
               [(<-str (take size bs)) (drop size bs)])))))

(defn decode-array [types bs]
  (loop [bs bs
         [x & xs] types
         acc []]
    (cond (and (empty? bs) (nil? x))
          acc

          (or (empty? bs) (nil? x))
          nil

          :else
          (when-let [[val bs] (decode-type x bs)]
            (recur bs xs (conj acc val))))))

(defn decode-hello [bs]
  (if (= bs (make-hello "pestcontrol" 1))
    [:msg {:type :hello
           :protocol "pestcontrol"
           :version 1}]
    [:error "bad hello"]))

(defn decode-error [bs]
  (let [mlen (<-u32 (take 4 (drop 5 bs)))]
    (if (not= mlen (- (count bs) 10))
      [:error "bad error"]
      [:msg {:type :error
             :error (<-str (take mlen (drop 9 bs)))}])))

(defn decode-ok [bs]
  (if (= bs (make-ok))
    [:msg {:type :ok}]
    [:error "bad ok"]))

(defn decode-target-populations [bs]
  (let [_mlen (<-u32 (take 4 (rest bs)))
        site (<-u32 (take 4 (drop 5 bs)))
        npops (<-u32 (take 4 (drop 9 bs)))]
    (if-let [pops (decode-array (flatten (repeat npops [:str :u32 :u32])) (butlast (drop 13 bs)))]
      [:msg {:type :target-populations
             :site site
             :populations (partition 3 pops)}]
      [:error "bad target-populations"])))

(defn decode-policy-result [bs]
  (let [mlen (<-u32 (take 4 (rest bs)))]
    (if (not= mlen 10)
      [:error "bad policy-result"]
      [:msg {:type :policy-result
             :policy (<-u32 (take 4 (drop 5 bs)))}])))

(defn valid-site-counts? [counts]
  (loop [[[k v] & kvs] counts
         m {}]
    (cond (nil? k) true
          (= (m k v) v) (recur kvs (assoc m k v))
          :else false)))

(defn decode-site-visit [bs]
  (let [_mlen (<-u32 (take 4 (rest bs)))
        site (<-u32 (take 4 (drop 5 bs)))
        npops (<-u32 (take 4 (drop 9 bs)))]
    (if-let [pops (decode-array (flatten (repeat npops [:str :u32])) (butlast (drop 13 bs)))]
      (let [counts (partition 2 pops)]
        (if (valid-site-counts? counts)
          [:msg {:type :site-visit
                 :site site
                 :populations (set counts)}]
          [:error "bad site-visit"]))
      [:error "bad site-visit"])))

(def decoder
  {0x50 #'decode-hello
   0x51 #'decode-error
   0x52 #'decode-ok
   0x54 #'decode-target-populations
   0x57 #'decode-policy-result
   0x58 #'decode-site-visit})

(defn decode [bs]
  (if (< (count bs) 5)
    [:need (- 5 (count bs))]
    (let [mtype (first bs)
          size (<-u32 (take 4 (rest bs)))]
      (cond (> size 100000)
            [:error "message too long"]

            (< (count bs) size)
            [:need (- size (count bs))]

            (valid-checksum? bs)
            (if-let [f (decoder mtype)]
              (f bs)
              [:error "bad packet"])

            :else
            [:error "bad checksum"]))))
