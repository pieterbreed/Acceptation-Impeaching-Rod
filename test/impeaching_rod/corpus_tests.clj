(ns impeaching-rod.corpus-tests
  (:use clojure.test
        impeaching-rod.rules
        impeaching-rod.corpus))

(defn make-many-result-docs
  "applies createfn n times and returns a sequence of the results"
  [n createfn]
  (->> (range n)
       (map createfn)))
  ;; (->> (range n)
  ;;      (partition 1 1 nil)
  ;;      (map #(map createfn %))
  ;;      flatten))
  










(run-all-tests #"impeaching-rod.corpus-tests")
