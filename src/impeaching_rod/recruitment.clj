(ns impeaching-rod.recruitment
  (:use impeaching-rod.common
        impeaching-rod.rules
        impeaching-rod.corpus))

(defonce db (create-corpus "recruitment db"))

(defonce names (slurp (clojure.java.io/resource "movie-characters.txt")))

(def skills #{:c# :c++ :java :c
              :haskell :clojure :f#
              :pascal :oberon
              :python :ruby
              :go :d :siebel :sql :oracle
              :unix :linux :windows :os2 :apple
              :objective-c :lisp :emacs
              :visual-studio :eclipse :vi
              :ecmascript :javascript})

(defn- get-person-skills
  "get at most 7 random skills from the skill pool"
  []
  (let [nr (int (* (rand) 7))]
    (take nr (->> skills
                  (sort-by (fn [_] (rand)))))))
  
(defn make-people
  "creates people profiles for the items in names"
  [n]
  (take n
        (map #(hash-map :name %1
                        :age %2
                        :skills (set %3))
             names
             (for [r (repeatedly rand)] (int (+ 20 (* r 40))))
             (repeatedly get-person-skills))))



