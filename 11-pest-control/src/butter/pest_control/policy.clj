(ns butter.pest-control.policy
  (:gen-class)
  (:require [butter.pest-control.client :refer [authority-client create-policy
                                                delete-policy site-targets]]
            [clojure.core.async :refer [<!! chan]]
            [clojure.core.match :refer [match]]))

(def num-policy-servers 16)
(def policy-channels (reduce #(assoc %1 %2 (chan)) {} (range num-policy-servers)))

(defn site-to-policy-channel [site]
  (policy-channels (mod (hash site) num-policy-servers)))

(defn watched-species [site]
  (keys (get @site-targets site)))

(defn normalize-population-count [site populations]
  (let [m1 (reduce #(assoc %1 %2 0) {} (watched-species site))
        m2 (reduce (fn [acc [k v]] (assoc acc k v)) {} populations)]
    (merge-with max m1 m2)))

(defn generate-policies [site populations]
  (loop [[[species n] & pops] (normalize-population-count site populations)
         acc []]
    (if (nil? species)
      acc
      (if-let [[pmin pmax] (get-in @site-targets [site species])]
        (cond (< n pmin)
              (recur pops (conj acc [species :conserve]))

              (> n pmax)
              (recur pops (conj acc [species :cull]))

              :else
              (recur pops (conj acc [species :none])))
        (recur pops acc)))))

(defn policy-deltas [site-policies site new-policies]
  (loop [[[species action] & ps] new-policies
         acc []]
    (if (nil? species)
      acc
      (let [[num prev-action] (get-in site-policies [site species])]
        (cond (= prev-action action)
              (recur ps acc)

              (and (nil? num)
                   (= action :none))
              (recur ps acc)

              (= action :none)
              (recur ps (conj acc [:delete species num]))

              (nil? num)
              (recur ps (conj acc [:create species action]))

              :else
              (recur ps (conj acc [:delete species num] [:create species action])))))))

(defn update-policies [site policies deltas]
  (loop [[d & ds] deltas
         acc policies]
    (if (nil? d)
      acc
      (match d
        [:delete s n]
        (do
          (delete-policy site s n)
          (recur ds (dissoc acc s)))

        [:create s a]
        (let [n (create-policy site s a)]
          (recur ds (assoc acc s [n a])))))))

(defn policy-server [ch]
  (loop [site-policies {}]
    (let [{:keys [site populations]} (<!! ch)]
      (authority-client site)
      (recur (->> populations
                  (generate-policies site)
                  (policy-deltas site-policies site)
                  (update-policies site (get site-policies site))
                  (assoc site-policies site))))))
