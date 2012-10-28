(ns impeaching-rod.rules-tests
  (:use clojure.test
        impeaching-rod.rules))

(deftest simple-matcher-test
  (testing "that simple matches work"
    ;; build simple matcher 
    (let [match (simple-matcher :value :value)]
      (are [exp req res] (= exp (match {:value req}
                                       {:value res}))
           
           1 1 1
           0 1 0))))

(deftest string-match-test
  (testing "that string-match works for valid cases"
    (let [match (string-matcher :v :v)]
      (are [exp req res] (= exp (match {:v req}
                                       {:v res}))

           1 "pieter" "pieter breed"
           1 "the" "in the thing"
           0 "here" "not in"))))

(deftest range-match-range-test
  (testing "how much two ranges overlap"
    (are [exp r1s r1e r2s r2e] (= exp
                                  (-measure-range-match {:start r1s :end r1e}
                                                        {:start r2s :end r2e}))
         0 1 2 3 4
         0 3 4 2 3
         1/2 0 4 2 5
         1 1 3 0 4)))

(deftest range-match-test
  (testing "whether points and ranges overlap properly"
    (let [match (range-matcher :v :v)]
      (are [exp req res] (= exp
                            (match {:v req}
                                   {:v res}))

         0 {:start 1 :end 3} 0
         0 {:start 1 :end 3} 1
         0 {:start 1 :end 3} 3
         0 {:start 1 :end 3} 4
         1 {:start 1 :end 3} 2
         
         0 0 {:start 1 :end 3} 
         0 1 {:start 1 :end 3} 
         0 3 {:start 1 :end 3} 
         0 4 {:start 1 :end 3} 
         1 2 {:start 1 :end 3}

         ;; sneaking in some range matching too
         ;; to test the binding to range matcher fn
         1/2 {:start 5 :end 10} {:start 5 :end 15}
         ))))
         
(deftest set-match-test
  (testing "how much sets overlap"
    (let [match (set-matcher :v :v)]
      (are [exp req res] (= exp
                            (match {:v req}
                                   {:v res}))
           0 [1 2 3] [4 5 6]
           1 [1 2 3] [1 2 3]
           1 [1 2 3] [1 2]
           0 [1 2 3] []
           0 [] []
           2/3 [1 2] [1 2 3]
           ))))

(deftest linear-fn-test
  (testing "whether linear interpolation between two points work"
    (are [exp x p1 p2] (= exp
                          ((-build-linear-function p1 p2) x))
         2 2 {:x 0 :y 0} {:x 5 :y 5}
         5 1 {:x 0 :y 6} {:x 2 :y 4}
         )))

(deftest scale-function-test
  (testing "whether the scaling function works like expecetd"
    (let [match (gliding-scale-matcher :v :v
                                       [{:x -10 :y 95}
                                        {:x 5   :y 30}
                                        {:x 40  :y 30}
                                        {:x 50  :y 50}])]
      (are [exp req res] (= exp
                            (match {:v req}
                                   {:v res}))

           95 -11 0
           95 -10 0
           95 -999 0
           62.5 -2.5 0
           30 5 0
           50 50 0
           50 1000 0

           ))))


(deftest matric-matcher-test
  (testing "whether the matrix matcher can work properly"
    (let [match (-build-matrix-matching-fn {:a {:1 1 :0 0}
                                            :b {:1 0 :0 1}})]
      (are [exp req res] (= exp
                            (match req res))

           1 :a :1
           0 :a :0
           0 :b :1
           1 :b :0
           ))))
                                            
(deftest weighted-table-test
  (testing "whether weighted table set matchers work"
    (let [match (weighted-set-matcher :v :v
                                      {:c++ {:java 1/2 :c# 1/2 :c++ 1 :haskell 0}
                                       :java {:java 1 :c# 1/2 :c++ 1/2 :haskell 0}
                                       :c# {:java 1/2 :c# 1 :c++ 1/2 :haskell 0}
                                       :haskell {:java 0 :c# 0 :c++ 0 :haskell 1}})]
      (are [exp req res] (= exp
                            (match {:v req}
                                   {:v res}))

           1 #{:c++} #{:c++}
           1 #{:java} #{:java}
           1 #{:java :c#} #{:c++}
           0 #{:java :c# :c++} #{:haskell}

           ))))

(deftest weighted-matcher-matcher-test
  (testing "that the weighted matcher matcher can accurately combine matchers"
    (let [match (weighted-matcher-matcher
                 identity identity
                 (string-matcher :name :name) 1
                 (simple-matcher :age :age) 9)]
    (are [exp req res] (= exp
                          (match req res))
         1/10
         {:name "pieter"
          :age 32}
         {:name "pieter"
          :age 30}

         9/10
         {:name "john"
          :age 32}
         {:name "pieter"
          :age 32}

         1
         {:name "pieter"
          :age 30}
         {:name "pieter"
          :age 30}
         
         ))))
         

(run-all-tests #"impeaching-rod.rules-tests")

