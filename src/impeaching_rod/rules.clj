(ns impeaching-rod.rules
  "holds matching rules builders. each builder takes at least two functions and optionally a builder parameter. The two functions (reqf resf) represent functions that query values from items that are being compare. The optional parameter is a parameter to the builder and makes sense based on the build. Each function returns a function that when given two items to compare (req res), knows how to extract the atributes from the two items (reqf req) and (resf res) and returns a score to indicate their match"
  (:use [impeaching-rod.common]))

(defmacro defmatcher
  "defines a matcher with the expected, standard matcher behaviour

takes a name, optional docstring, optional attribute map and 3 required parameters:
1. a vector of extra parameters (over and above reqf & reqs which all matchers take)
2. a vector of binding forms, which may make use of the extra params. These binging forms will create symbols in the scope of the third argument...
3. a function takeing only exactly 2 parameters (req and res) which returns the value of the match between the [req]ust and the [res]ult records

eg
; (defmatcher simple-matcher
; \"docstring abc 123\"
;  [] ;; taking no extra parameters
;  [] ;; no extra binding forms
;  #(= %1 %2)) ;; anonymous function that takes two parameters (those are the values of the rquest and result records that are being compared) and does simple equality testing against them.
; (doc simple-matcher) --> \"docstring abc 123\"

eg2
; (defmatcher both-is-n-matcher
;  [n] ;; taking a value n, which both properties must be equal to
;  [compfn %(= n %1 %2)] ;; defines a function called compfn, takes 2 parameters, compares both against the value given in n and determines if they are all the same
;  #(compfn %1 %2)) ;; anonymous function that takes two parameters (those are the values of the request and result records and applies it against the compfn function and returns the result of that"
  [name & args]
  (let [[name args] (name-with-attributes name args)
        [pars lets matching-fn rest] args]
        
    `(do (defn ~name [reqf# resf# ~@pars]
           (let [~@lets]
             (fn [req# res#]
               (let [req*# (reqf# req#)
                     res*# (resf# res#)]
                 (~matching-fn req*# res*#)))))
         (alter-meta! (var ~name) assoc :arglists
                      '([~(symbol "reqf") ~(symbol "resf")
                         ~@pars]))
         ~name)))
(alter-meta! #'defmatcher assoc :arglists
             '([name doc-string? attr-map? [pars*] [lets*] matching-fn]))

(defmatcher simple-matcher
  "gives (fn [req res]) so that it matches when (= (reqf req) (resf res)), ie, simple value-based equality

eg (def x {:age 25}
   (def y {:query-age 25})
   (def matcher (simple-matcher :query-age :age))
   (matcher y x) -> 1"
  [] []
  #(if (= %1 %2)
     1 0))

(defmatcher string-matcher
  "gives (fn [req res] ...) so that it matches when req is contained in res"
  [] []
  #(if (= %1
          (re-find (re-pattern %1)
                   %2))
     1 0))
  
(defn -measure-range-match
  "matches the percentage of how much of the range of r1 is in the range of r2"
  [r1 r2]
  {:pre [(every? range? [r1 r2])]}
  
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

(defmatcher range-matcher
  "matches:
- a point to a range
  - if a is in range, then 1.0, else 0.0
- a range to a point
  - if range contains point, then 1.0, else 0.0
- two ranges
  - if req is wholly in res, then 1.0, else a the percentage of req in res

a range is a map with keys :start and :end. Values are anything which supports <, > and - operators against the values"
  [] []
  #(cond
    ;; two ranges
    (and (range? %1)(range? %2)) (-measure-range-match %2 %1)

    ;; point and range
    (range? %1) (if (and (< %2 (:end %1))
                         (> %2 (:start %1)))
                  1 0)

    ;; range and point
    (map? %2) (if (and (< %1 (:end %2))
                       (> %1 (:start %2)))
                1 0)

    :else 0))

(defmatcher set-matcher
  "creates sets out of the two collections (reqf req) and (resf res). The result is the proportion of items in %2 that is also in %1"
  [] []
  #(let [nrres (count %2)]
     (if (= 0 nrres)
       0
       (-> (clojure.set/intersection (toset %1)
                                     (toset %2))
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

(defn -build-gliding-scale-function
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

;[{:x -10 :y 95}
; {:x 5   :y 30}
; {:x 40  :y 30}
; {:x 40  :y 50}]"
  [ranges]
  {:pre [(and (vector? ranges)
              (every? point? ranges))]}
  (let [rangess (sort-by :x ranges)
        ranges (-> (for [i (range (dec (count rangess)))]
                     (let [p1 (nth ranges i)
                           p2 (nth ranges (inc i))]
                     [(:x p1) (:x p2) (-build-linear-function p1 p2)]))
                   vec)]
    (fn [x]
      (let [frst (-> rangess first)
            lst (-> rangess last)]
        (if (<= x (:x frst)) ; smaller than smallest x
          (:y frst)
          (let [fnd (some #(if (= x (:x %)) %) rangess)] ; exactly as one of the x's
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

(defmatcher gliding-scale-matcher
  "matches the difference between (reqf req) and (resf res) and uses that as a lookup into gliding scale function that is specified with the par parameter. par is specified as a table with value -> match entries. Once the table is sorted on value, interpolation is used to compute the function values inbetween the table points"
  [par]
  [match-fn (-build-gliding-scale-function par)]

  #(match-fn (- %1 %2)))

(defn -build-matrix-matching-fn
  "takes a table (map of maps) and gives a fn that can look up a value from the table, provided that the param keys are values in the maps"
  [tbl]
  (fn [req res]
    (-> tbl
        req
        res)))

(defmatcher matrix-rule-matcher
  "matches two values against one another according to a lookup table keyd by the values themselves"
  [tbl]
  [matcher (-build-matrix-matching-fn tbl)]
  #(matcher %1 %2))

(defmatcher weighted-set-matcher
  "matches two sets against one another. Takes a table that defines weights of how the matching elemenents match against each other. Adds up all of the weights of the matching items and returns a value no bigger than 1. eg:

; tbl: {:c++     {:java 1/2 :c# 1/2 :c++ 1/2}
;       :java    {:java 1   :c# 1   :c++ 1/2}
;       :c#      {:java 1/2 :c# 1   :c++ 1/2}
;       :haskell {:java 0   :c# 0   :c++ 0  }}
;
; will match 1 on #{:java} #{:java}
; also       1 on #{:c++ :c#} #{:java}
; also       1 on #{:c++ :c#} #{:c++}
: but        0 on #{:haskell} #{:java}"
  [tbl]
  [tbl-match (-build-matrix-matching-fn tbl)]
  #(->> (for [rq %1 rs %2] (tbl-match rq rs))
        (apply +)
        max))

(defmatcher weighted-matcher-matcher
  "allows the ability to combine matchers based on relative weightings. takes parameters in groups of 2 (ie will error out of there are not a multiple of 2 nr of parameters) For every group:

- the first is the matcher (simple-matcher :name :name)
- the second is the relative weight, as a number

; eg, to match for someone named 'John' with age exactly '30', with age being 9 times more important the name
; (weighted-matcher-matcher (string-matcher :name :name) 1
;                           (simple-matcher :age :age) 9)"
  [& matchers]
  [parts (partition 2 matchers)
   _ (debug-pprint parts)
   totalweight (apply + (map second parts))
   _ (debug-pprint totalweight)

   ;; takes one group of the matchers
   ;; and makes a function that can be invoked with the
   ;; req and the res, but also combines that result
   ;; with the relative weight of that matcher
   compute-match-fn (fn [part]
                      (let [matcher (first part)
                            weight (second part)]
                        (fn [req res]
                          (let [match (matcher req res)
                                weight-part (/ weight totalweight)
                                result (* match weight-part)]
                            result))))

   match-fns (map compute-match-fn parts)]
  (fn [req res]
    (->> match-fns
        (map #(apply % [req res]))
        (apply +))))


