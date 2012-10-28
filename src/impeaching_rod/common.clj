(ns impeaching-rod.common
  "common utility functions used in the impeach rod")

(def BATCH-SIZE
  "This is the batch size as used by request. The documents in a corpus will be
split into separate collections of this size and the request will be run concurrently
on these separate document collections."
  1000)

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

(defn abs
  "absolute value"
  [x]
  (if (neg? x)
    (* -1 x)
    x))

(defn toset
  "transforms a collection into a set of items"
  [x]
  (if (set? x) x
      (if (coll? x) (set x)
          #{})))

(defn split-into-job-coll
  "takes a collection and partitions it into sub collections so that the sequences can be processed in batches"
  [size coll]
  (->> coll
       (partition size size nil)
       (seque 10)))

(defn map-in-jobs
  "exactly like map, but splits the collection into sub collections, and run the fn in map mode on the sub collections"
  [fn col]
  (let [result (->> (split-into-job-coll BATCH-SIZE col)
                    (map #(map fn %)))]
    (for [x result y x] y))) ; fancy flatten-once

(defn name-with-attributes
  "To be used in macro definitions.
   Handles optional docstrings and attribute maps for a name to be defined
   in a list of macro arguments. If the first macro argument is a string,
   it is added as a docstring to name and removed from the macro argument
   list. If afterwards the first macro argument is a map, its entries are
   added to the name's metadata map and the map is removed from the
   macro argument list. The return value is a vector containing the name
   with its extended metadata map and the list of unprocessed macro
   arguments."
  [name macro-args]
  (let [[docstring macro-args] (if (string? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [nil macro-args])
    [attr macro-args]          (if (map? (first macro-args))
                                 [(first macro-args) (next macro-args)]
                                 [{} macro-args])
    attr                       (if docstring
                                 (assoc attr :doc docstring)
                                 attr)
    attr                       (if (meta name)
                                 (conj (meta name) attr)
                                 attr)]
    [(with-meta name attr) macro-args]))


(defn debug-pprint [x]
  (clojure.pprint/pprint x)
  x)