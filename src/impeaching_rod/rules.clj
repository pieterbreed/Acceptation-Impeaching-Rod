(ns impeaching-rod.rules
  "holds matching rules.

each rule takes a documented number of arguments and returns a value. return values are between 0.0 and 1.0 with 0.0 meaning does not match at all and 1.0 meaning matches perfectly"
  )

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
        