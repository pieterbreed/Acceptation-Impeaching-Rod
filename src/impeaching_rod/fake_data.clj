(ns impeaching-rod.fake-data)

(defn read-lines-from-resource [f]
  (->> (clojure.java.io/resource f)
       slurp
       clojure.string/split-lines))

(def names (read-lines-from-resource "movie-characters.txt"))
(def surnames (read-lines-from-resource "surnames.txt"))

(defn make-names
  "makes random names from the names and surnames lists"
  []
  (let [modfn #(->> % (sort-by (fn [_] (rand))) cycle)
        n (modfn names)
        s (modfn surnames)]
    (->> (interleave n s)
         (partition 2)
         (map #(str (first %) " " (second %))))))

(def skills #{:c# :c++ :java :c
              :haskell :clojure :f#
              :pascal :oberon
              :python :ruby
              :go :d :siebel :sql :oracle
              :unix :linux :windows :os2 :apple
              :objective-c :lisp :emacs
              :visual-studio :eclipse :vi
              :ecmascript :javascript
              :zeromq :rabbitmq
              :amqp :qpid
              :weblogic :soasuite})



(defn- make-skills-list
  "get at most nr random skills from the skill pool"
  [n]
  (let [nr (int (* (rand) n))]
    (take nr (->> skills
                  (sort-by (fn [_] (rand)))))))

(defn- make-salary
  "creates a salary no lower than 100 000 and now higher than 700 000"
  []
  (->> (rand)
       (* 600000)
       (+ 100000)
       int))
  
(defn make-people
  "creates people profiles"
  []
  (map #(hash-map :name %1
                  :age %2
                  :skills (set %3)
                  :salary %4)
       (make-names)
       (for [r (repeatedly rand)] (int (+ 20 (* r 40))))
       (repeatedly #(make-skills-list 7))
       (repeatedly make-salary)))


  