(ns impeaching-rod.common
  "common utility functions used in the impeach rod")

(defn point?
  "tests a map x whether it has keys :x and :y"
  [x]
  (and (map? x)
       (contains? x :x)
       (contains? x :y)))

(defn debug-pprint [x]
  (clojure.pprint/pprint x)
  x)