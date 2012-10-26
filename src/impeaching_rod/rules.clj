(ns impeaching-rod.rules
  "holds matching rules. each rule takes a documented number of arguments and returns a value. return values are between 0.0 and 1.0 with 0.0 meaning does not match at all and 1.0 meaning matches perfectly"
  (:use [impeaching-rod.common]))

(defn simple-match
  "matches when (= req res), ie, simple value-based equality"
  [req res]
  (if (= req res)
    1
    0))

(defn string-match
  "matches 1.0 when req is in res, otherwise 0.0"
  ([req res]
     (if (= req
            (re-find (re-pattern req)
                     res))
       1
       0)))

(defn -range-match-ranges [r1 r2]
  "matches the percentage of how much of the range of r1 is in the range of r2"
  (let [{r1start :start, r1end :end} r1
        {r2start :start, r2end :end} r2]
    (cond
     ;; r1: <------->
     ;; r2:            <---->
     (<= r1end r2start) 0

     ;; r1          <----->
     ;; r2 <----->
     (<= r2end r1start) 0

     true (let [r1size (- r1end r1start)
                s (max r1start r2start)
                e (min r1end r2end)
                inside (- e s)]
            (/ inside r1size)))))

(defn range-match
  "matches:
- a point to a range
  - if a is in range, then 1.0, else 0.0
- a range to a point
  - if range contains point, then 1.0, else 0.0
- two ranges
  - if req is wholly in res, then 1.0, else a the percentage of req in res

a range is a map with keys :start and :end. Values are anything which supports < and > against the values"
  [req res]
  (cond
   ;; two ranges
   (and (map? req)(map? res)) (-range-match-ranges res req)

   ;; point and range
   (map? req) (if (and (< res (:end req))
                       (> res (:start req)))
                1 0)

   ;; range and point
   (map? res) (if (and (< req (:end res))
                       (> req (:start res)))
                1 0)

   :else 0))

(defn set-match
  "creates sets out of the two seqs req and res. The result is the proportion of items in res that is also in req"
  [req res]
  (let [toset #(if (set? %)
                 %
                 (if (coll? %)
                   (set %)
                   #{}))
        reqset (toset req)
        resset (toset res)
        nrres (count resset)]
    (if (= 0 nrres)
      0
      (-> (clojure.set/intersection reqset resset)
          count
          (/ nrres)))))



(defn -build-linear-function
  "creates a function that gives the y value for a line that goes through p1 and p2 in 2d space. p1 and p2 are maps that have :x and :y keys"
  [p1 p2]
  {:pre [(and (point? p1)
              (point? p2))]
   :post [(fn? %)]}
  (let [{x1 :x y1 :y} p1
        {x2 :x y2 :y} p2]
    (fn [x]
      {:pre [(and (< x x2)
                  (< x1 x))]
       :post [(and (< % (max y1 y2))
                   (< (min y1 y2) %))]}
      (+ y1
         (* (- x x1)
            (/ (- y2 y1)
               (- x2 x1)))))))

(defn -build-scale-function
  "returns a function that will use interpolation to map input -> output:

95 ______________
                 |\\
                 | \\
                 |  \\         ______________ 50
                 |   \\       /|                     
                 |    \\_30__/ |                         
                 |    |     |  |                      
-----------------|----|-----|-|--------------
                -10   5    40  50           

eg -11 -> 95
   -10 -> 95
  -999 -> 95
  -2.5 -> 62.5
     5 -> 30
    50 -> 50
 10000 -> 50

and so on. In this example, req will be specified as:

[{:x -10 :y 95}
 {:x 5   :y 30}
 {:x 40  :y 30}
 {:x 40  :y 50}]"
  [req]
  {:pre [(and (vector? req)
              (every? point? req))]}
  (let [reqs (sort-by :x req)
        ranges (-> (for [i (range (dec (count reqs)))]
                     (let [p1 (nth req i)
                           p2 (nth req (inc i))]
                     [(:x p1) (:x p2) (-build-linear-function p1 p2)]))
                   vec)]
    (fn [x]
      (let [frst (-> reqs first)
            lst (-> reqs last)]
        (if (<= x (:x frst)) ; smaller than smallest x
          (:y frst)
          (let [fnd (some #(if (= x (:x %)) %) reqs)] ; exactly as one of the x's
            (if fnd
              (:y fnd)
              (if (>= x (:x lst)) ; larger than biggest x
                (:y lst)
                (-> (filter #(and (> x (first %))
                                  (< x (second %)))
                            ranges)
                     first
                     (nth 2)
                     (apply x []))))))))))

(defn scale-match
  "matches the res value against a function (req). req is specified as a table with value -> match entries. Once the table is sorted on value, interpolation is used to compute the function values inbetween the table points"
  []
  nil)
  
        