(ns impeaching-rod.rules-tests
  (:use clojure.test
        impeaching-rod.rules))

(deftest simple-match-test
  (testing "that simple matches work"
    (are [exp req res] (= exp (simple-match req res))

         1 1 1
         0 1 0)))

(deftest string-match-test
  (testing "that string-match works for valid cases"
    (are [exp req res] (= exp (string-match req res))

         1 "pieter" "pieter breed"
         1 "the" "in the thing"
         0 "here" "not in")))

(deftest range-match-range-test
  (testing "how much two ranges overlaps"
    (are [exp r1s r1e r2s r2e] (= exp
                                  (-range-match-ranges {:start r1s :end r1e}
                                                       {:start r2s :end r2e}))
         0 1 2 3 4
         0 3 4 2 3
         1/2 0 4 2 5
         1 1 3 0 4)))

(deftest range-match-test
  (testing "whether points and ranges overlap properly"
    (are [exp req res] (= exp
                          (range-match req res))

         0 {:start 1 :end 3} 0
         0 {:start 1 :end 3} 1
         0 {:start 1 :end 3} 3
         0 {:start 1 :end 3} 4
         1 {:start 1 :end 3} 2
         
         0 0 {:start 1 :end 3} 
         0 1 {:start 1 :end 3} 
         0 3 {:start 1 :end 3} 
         0 4 {:start 1 :end 3} 
         1 2 {:start 1 :end 3} )))
         
(deftest set-match-test
  (testing "how much sets overlap"
            (are [exp req res] (= exp
                                  (set-match req res))
                 0 [1 2 3] [4 5 6]
                 1 [1 2 3] [1 2 3]
                 1 [1 2 3] [1 2]
                 0 [1 2 3] []
                 0 [] []
                 2/3 [1 2] [1 2 3]
                 )))

(deftest linear-fn-test
  (testing "whether linear interpolation between two points work"
    (are [exp x p1 p2] (= exp
                          ((-build-linear-function p1 p2) x))
         2 2 {:x 0 :y 0} {:x 5 :y 5}
         5 1 {:x 0 :y 6} {:x 2 :y 4}
         )))

(deftest scale-function-test
  (testing "whether the scaling function works like expecetd"
    (are [exp x] (= exp
                    ((-build-scale-function [{:x -10 :y 95}
                                             {:x 5   :y 30}
                                             {:x 40  :y 30}
                                             {:x 50  :y 50}])
                     x))

         95 -11
         95 -10
         95 -999
         62.5 -2.5
         30 5
         50 50
         50 10000

         )))
                                            


(run-all-tests #"impeaching-rod.rules-tests")

