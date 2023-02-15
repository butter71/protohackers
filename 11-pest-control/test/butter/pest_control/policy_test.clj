(ns butter.pest-control.policy-test
  (:require [butter.pest-control.client :refer [site-targets]]
            [butter.pest-control.policy :refer :all]
            [clojure.test :refer :all]))

(deftest policies-test
  (reset! site-targets {12345 {"aaa" [5 10]
                               "bbb" [10 20]
                               "ccc" [15 30]
                               "ddd" [1 5]}})
  (testing "generate list of policy deltas"
    (let [populations #{["aaa" 3] ["bbb" 12] ["ccc" 35]}
          site-policies {12345 {"aaa" [0 :cull]
                                "bbb" [2 :conserve]}}]
      (is (= [[:delete "aaa" 0]
              [:create "aaa" :conserve]
              [:delete "bbb" 2]
              [:create "ccc" :cull]
              [:create "ddd" :conserve]]
             (->> populations
                  (policy-deltas site-policies 12345)))))))
