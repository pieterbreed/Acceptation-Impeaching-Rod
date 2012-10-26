(ns impeaching-rod.common
  "common utility functions used in the impeach rod")

(defn point?
  "tests a map x whether it has keys :x and :y"
  [x]
  (and (map? x)
       (contains? x :x)
       (contains? x :y)))

(defn range?
  "tests whether x is a range, ie, is a map and has keys :start and :end"
  [x]
  (and (map? x)
       (contains? x :start)
       (contains? x :end)))

(defn debug-pprint [x]
  (clojure.pprint/pprint x)
  x)