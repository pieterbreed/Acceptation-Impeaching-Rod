(ns impeaching-rod.corpus-tests
  (:use clojure.test
        impeaching-rod.common
        impeaching-rod.rules
        impeaching-rod.corpus))

(defn- make-many-result-docs
  "applies createfn n times and returns a sequence of the results"
  [n createfn]
  (->> (range n)
       (map-in-jobs createfn)))

(defn- make-test-corpus
  "makes a test corpus by calling make-many-result-docs on the passed fn"
  [n createfn]
  (let [c (create-corpus "test")]
    (add-documents c (make-many-result-docs n createfn))
    c))
  
(deftest simple-request-test
  (testing "small nr of simple documents, doing simple matching"
    (let [createfn #(hash-map :name (str "name" %) :age %)
          corpus (make-test-corpus 11 createfn)
          matcher (simple-matcher :age :age)
          find-result #(->> (request corpus matcher %)
                            (sort-by :score)
                            last ;; top reults are at the end, sort goes low-to-high
                            :result)]
      (are [exp req] (= exp
                        (find-result req))

           (createfn 10) {:age 10}
           (createfn 1) {:age 1}
           ))))


(run-all-tests #"impeaching-rod.corpus-tests")
