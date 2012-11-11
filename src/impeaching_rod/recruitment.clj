(ns impeaching-rod.recruitment
  (:use impeaching-rod.common
        impeaching-rod.rules
        impeaching-rod.corpus
        impeaching-rod.fake-data))

;; def defines a binding (gives a value to a symbol)

;; this binding is the result of creating a corpus
;; with 10 000 candidate resumes
;; the corpus is the thing that holds the data
;; and also other things about the data
;; eg it could hold indexes for each 
;; matcher/result pair (not implemented)
;; create-corpus is a function in corpus.clj
;; make-people is a function in fake_data.clj
(def candidates (let [c (create-corpus "candidate db")]
                  (add-documents c (take 10000 (make-people)))
                  c))

;; defines a matcher that we will use below.
;; it knows how to extract a value from both the
;; request record (1st parameter) and from the
;; result record (2nd parameter). When a comparison
;; must be made, the request parameter is transform
;; and so is the result record and the result values
;; will be compared to determine the match value
;; the simple matcher performs an equality test
;; if the two values are equal the match value is
;; 1 (highest) if not 0 (lowest)
(def matcher-age-equals
  (simple-matcher identity ;; means the request record is it's own value
                  :age))   ;; means use the age value from the result record

;; defn creates a function
;; filter-full-matches-only is the name of the symbol
;; [xs] implies one parameter, called 'xs'
;; the string after the symbol is the symbol's doc-string
;; which can be queried on the console using
;; (doc symbol-name)
(defn filter-full-matches-only
  "Filters out any record where the :score value is not equal to 1"
  [xs]
  (filter #(= 1 (:score %)) xs))

(defn map-out-result-records
  "Returns the value of the :result field for each record"
  [xs]
  (map :result xs))

;; request is defined in the corpus package
;; it takes the corpus itself, a matcher and a request record
;; find-everyone-of-age also filter based on the matching value
;; in this case 1 means full match and 0 means no match at all
;; which is what you want for an age equality matcher
;; then lastly it maps the corpus document (the thing you searched for)
;; out of the match-result record
;; match result records contains the request document, the corpus doc
;; and the score

;; the ->> macro does a similar thing as f#'s |> operator
;; it's defined as: (->> (fn-name param-1 param-2 param-3)
;;                       (fn-name2 param-4 param-5))
;; is               (fn-name2 param-4 param-5 (fn-name param-1 param-2 param-3))
;; ie, it insert the earlier binding into the last parameter slot

(defn find-everyone-of-age
  "finds all the candidates that has a specific age in the candidates corpus"
  [age]
  (->> (request candidates
                matcher-age-equals
                age)
       filter-full-matches-only
       map-out-result-records))

;; eg (take 10 (find-everyone-of-age 25))
;; 
;; ({:age 25, :skills #{}, :name "Beissart Rathbone"}
;;  {:age 25, :skills #{:unix}, :name "Bettis Earl"}
;;  {:age 25, :skills #{}, :name "Favor Kurtz"}
;;  {:age 25, :skills #{}, :name "Bunthorne Makins"}
;;  {:age 25,
;;   :skills #{:python :qpid :eclipse :soasuite},
;;   :name "Gedge Allcock"}
;;  {:age 25,
;;   :skills #{:python :qpid :soasuite :emacs :windows :c++},
;;   :name "Aid Arnott"}
;;  {:age 25, :skills #{:c}, :name "Howells Ratcliffe"}
;;  {:age 25,
;;   :skills #{:python :qpid :eclipse :soasuite},
;;   :name "Hartford Tenison"}
;;  {:age 25, :skills #{:emacs :windows :go}, :name "Astor Swidenbank"}
;;  {:age 25,
;;   :skills #{:qpid :soasuite :weblogic :c :unix :apple},
;;   :name "Arena Dobson"})

(def matcher-name-contains
  "Matches records where the :name contains a specified string"
  (string-matcher identity
                  :name))

(defn find-everyone-with-name-part
  "finds all candidates that has 'name' as part of their name"
  [name]
  (->> (request candidates
                matcher-name-contains
                name)
       filter-full-matches-only
       map-out-result-records))

;; eg (take 10 (find-everyone-with-name-part "john"))
;; 
;; ({:age 29, :skills #{}, :name "Applejohn Procter-baines"}
;;  {:age 57,
;;   :skills #{:eclipse :pascal :javascript},
;;   :name "Deanna Meikeljohn"}
;;  {:age 35, :skills #{:javascript}, :name "Ladislaw Littlejohn"}
;;  {:age 35,
;;   :skills #{:python :qpid :soasuite},
;;   :name "Egerman Goodsell-johnson"}
;;  {:age 32, :skills #{:python :soasuite}, :name "Bailez St john"}
;;  {:age 40, :skills #{:siebel :rabbitmq}, :name "Fitzjohn Coggan?"}
;;  {:age 43, :skills #{:qpid :unix :apple :d :vi}, :name "Kalle St john"}
;;  {:age 23, :skills #{:qpid :os2}, :name "Claw Goodsell-johnson"})

(def matcher-age-range
  "defines a matcher that can match against age range"
  (range-matcher identity
                 :age))

(defn find-everyone-of-age-between
  "finds everyone between the ages of a and b"
  [a b]
  (->> (request candidates
                matcher-age-range
                {:start a :end b})
       filter-full-matches-only
       map-out-result-records))

;; eg (take 10 (find-everyone-of-age-between 28 32 ))
;; 
;; ({:age 31, :skills #{}, :name "Falin Cleaver"}
;;  {:age 29,
;;   :skills #{:python :eclipse :c :apple},
;;   :name "Beardsley Haywood"}
;;  {:age 29, :skills #{:qpid :c++}, :name "Beaudricourt Mercer"}
;;  {:age 30,
;;   :skills #{:python :eclipse :emacs :windows :c++ :go},
;;   :name "Goebel Ormeston"}
;;  {:age 29,
;;   :skills #{:eclipse :linux :java :ecmascript},
;;   :name "Jachino Cullison"}
;;  {:age 30, :skills #{}, :name "Flannery Gowlland"}
;;  {:age 29,
;;   :skills #{:python :eclipse :siebel :d},
;;   :name "Acne Jewison"}
;;  {:age 31,
;;   :skills #{:qpid :eclipse :siebel :d :pascal},
;;   :name "Jablonski Boyer"}
;;  {:age 29, :skills #{:python :zeromq}, :name "CID Gell"}
;;  {:age 30,
;;   :skills #{:eclipse :siebel :d :windows :c++},
;;   :name "Esteban Boddy"})

;; functions can be created anonymously in two ways
;; #(fn %) is a short-form function creator.
;; #() implies the in-line fn and % or %1, %2 are the parameters
;; the more normal way is (fn [...]) which used by the 'defn' macro
;; it's used like this (def fn-name (fn [p1 p2] (+ p1 p2)))
;; which binds a function adding two parameters to a symbol fn-name

(defn sort-on-match-score
  "sorts match result documents based on the match score, highest to lowest"
  [results]
  (->> results
       (sort-by (fn [r] (:score r)))
       reverse))

;; defines a matcher that knows how to rake the intersection of two sets
(def matcher-sets
  (set-matcher identity
               :skills))

(defn find-and-rank-people-with-skills
  "Specifies a set of skills and ranks candidates based on how closely their skillset matches the required skillset"
  [skillset]
  {:pre [(set? skillset)]} ;; pre-condition, skillset must be a set
  (->> (request candidates
                matcher-sets
                skillset)
       sort-on-match-score
       (map #(select-keys % [:score :result]))))

;; eg demonstrating pre-condition failure
;; (take 10 (find-and-rank-people-with-skills {:test 0})))
;; AssertionError Assert failed: (set? skillset)  impeaching-rod.recruitment/find-and-rank-people-with-skills (recruitment.clj:119)

;; (take 10 (find-and-rank-people-with-skills #{:zeromq :python :unix :go}))
;; 
;; ({:result
;;   {:age 32,
;;    :skills #{:python :unix :go :zeromq},
;;    :name "Augustine Snoddy"},
;;   :score 1}
;;  {:result
;;   {:age 46,
;;    :skills #{:python :qpid :windows :go :zeromq :c#},
;;    :name "Acaro Dyson"},
;;   :score 3/4}
;;  {:result
;;   {:age 50,
;;    :skills #{:python :go :oberon :zeromq :c#},
;;    :name "McCulloch Downer"},
;;   :score 3/4}
;;  {:result
;;   {:age 58,
;;    :skills #{:python :apple :go :zeromq :sql :linux},
;;    :name "Lorinda Steeple"},
;;   :score 3/4}
;;  {:result
;;   {:age 26,
;;    :skills #{:python :soasuite :go :zeromq},
;;    :name "Ivanhoe Tanson"},
;;   :score 3/4})

;; at this point in developing this example I added a :salary field to the
;; recruitment corpus documents to demonstrate some of the example below
;; that is why the example output above does not include this field

(defn- get-avg-salary
  "Determines the avarage salary of all candidates"
  []
  (->> (:documents @candidates) ;; get all documents from the shared atom candidates
       (map :salary)
       (reduce (fn [aggr s] {:total (+ (:total aggr) s) :nr (inc (:nr aggr))})
               {:total 0
                :nr 0})
       (#(/ (:total %)
            (:nr %)))
       int))

;; running this on the data on my machine I consistently got values
;; close to 400 000, which makes sense given a normal distribution
;; and how I generated salary data



(defn find-people-who-will-be-happy-with-salary
  "finds people who will be happy with a certain salary. Happiness here is defined like this: if the candidate's salary expectation is lower than or equal to the requested value, happiness is 100%. Then gradually is the candidates's expectation becomes more and more than the requested salary, the happiness level decreases until at 'tolerance' level away, the candidate is completely unhappy:

eg Tanaka expects    75 000
   Pieter expects   100 000
   David expects    110 000
   Ulvi expects     125 000
   Michael expects  150 000

if we parameterize this method with 100000 and 30000 (requested and tolerance respectively) then Tanaka & Pieter will be completely happy, David will be less happy than Pieter and Tanaka, Ulvi will be very unhappy and Michael will be completely unhappy"
  [salary tolerance]
  (->> (request candidates
                (gliding-scale-matcher identity
                                       :salary
                                       [{:x (* -1 tolerance) :y 0} ;; explanation given below
                                        {:x 0 :y 100}])
                salary)
       sort-on-match-score
       (map #(select-keys % [:score :result]))))

;; the 3rd parameter to gliding-scale-matcher is a table that specifies ranges
;; on an x scale. Points on the x-scale are given a y-value. linear interpolation is used
;; to determine a y value between specified x values. The way this match fn works
;; is the y-value is the ultimate match score of the record. An x-value is computed
;; by subtracting the request and result documents' values from each other
;;
;; in this case, since the candidates's salary is subtracted from the requested salary
;; to determine the x-value, values lower than 0 implies the candidate expects more
;; than what is on offer



;; here I had to hunt near the top of the allowed salary range (700 000) to find
;; a good demonstration of this mechanism. I'm searching for people who will
;; be happy with 695k with a tolerance of 5k, which means the closer you get to
;; 700k the unhappier you get, untit at 695k you're completely happy.
;; I reversed the normal result set so we can see the scores
;; increasing, slowly as the salary becomes more and more into expectation

;; (->> (find-people-who-will-be-happy-with-salary 695000 5000) reverse (drop 20) (take 10) pprint)
;; ({:result
;;   {:age 56,
;;    :skills #{:python},
;;    :name "Bockstiegel Dibnah",
;;    :salary 699053},
;;   :score 947/5000}
;;  {:result
;;   {:age 57,
;;    :skills #{:python},
;;    :name "Baer Illingworth",
;;    :salary 698901},
;;   :score 1099/5000}
;;  {:result
;;   {:age 28,
;;    :skills #{:python :eclipse :ruby :d :vi :pascal},
;;    :name "Foresi Fereday",
;;    :salary 698890},
;;   :score 111/500}
;;  {:result
;;   {:age 46, :skills #{}, :name "Beckerath Courte", :salary 698815},
;;   :score 237/1000}
;;  {:result
;;   {:age 46,
;;    :skills #{:qpid :ruby},
;;    :name "Collector Pill",
;;    :salary 698765},
;;   :score 247/1000}
;;  {:result
;;   {:age 56,
;;    :skills #{:c++ :oberon},
;;    :name "Bainter Hudgell",
;;    :salary 698740},
;;   :score 63/250}
;;  {:result
;;   {:age 37,
;;    :skills #{:c++ :linux},
;;    :name "Devlin Venn",
;;    :salary 698691},
;;   :score 1309/5000}
;;  {:result
;;   {:age 46, :skills #{}, :name "Corven Shepshed", :salary 698657},
;;   :score 1343/5000}
;;  {:result
;;   {:age 21,
;;    :skills #{:ruby :clojure},
;;    :name "Hansig Mack
;; ",
;;    :salary 698629},
;;   :score 1371/5000}
;;  {:result
;;   {:age 29,
;;    :skills #{:soasuite :visual-studio :emacs :windows},
;;    :name "Clergyman Pettifer",
;;    :salary 698472},
;;   :score 191/625})



(defn find-people-with-skills-close-to
  "Uses a table to correlate skills with each other and matches candidates based on this heuristic. eg, java and c# and c++ may be considered 'close' skills. So too something like qpid and amqp. The table lookup value implies exactly how close two skills are to each other. Then we use this information to search for candidates with skills close to the skills we are looking for. If one candidate has more than one skill 'close' to the skills we are looking for they are added up to a max of 1 (Ie perfect match)"
  [skill-table skill]
  (->> (request candidates
                (weighted-set-matcher identity
                                      :skills
                                      skill-table)
                skill)
       sort-on-match-score
       (map #(select-keys % [:score :result]))))

;; this is an example skills correlation matrix
;; that matches c-style skillsets with each other
(def c-style-skills-matrix
  {:c# {:c++ 2/4 :java 3/4 :c 1/4 :c# 1}
   :java {:c++ 1/2 :java 1 :c 1/4 :c# 3/4}
   :c++ {:c# 1/8 :java 1/8 :c 3/4 :c++ 1}
   :c {:c# 1/2 :java 1/2 :c++ 3/4 :c 1}})

;; to take the first example below
;; this candidate has a skill of :c, we are looking for :c++
;; if we look that up on the table :c and :c++ correlates at 3/4 score
;; because that is how we set it up

;; eg (->> (find-people-with-skills-close-to c-style-skills-matrix #{:c++})
;; 					 (drop 1000)
;; 					 (take 10))
;; ({:result
;;   {:age 31,
;;    :skills #{:c :objective-c},
;;    :name "Gould Astle",
;;    :salary 170254},
;;   :score 3/4}
;;  {:result
;;   {:age 24,
;;    :skills #{:soasuite :c :apple :d},
;;    :name "Costillo Mohr",
;;    :salary 178582},
;;   :score 3/4}
;;  {:result
;;   {:age 36,
;;    :skills #{:soasuite :c :vi :clojure},
;;    :name "Greeting Farrier",
;;    :salary 180208},
;;   :score 3/4}
;;  {:result
;;   {:age 31,
;;    :skills #{:qpid :c :os2},
;;    :name "Beinstock Osburn",
;;    :salary 510230},
;;   :score 3/4}
;;  {:result
;;   {:age 36,
;;    :skills #{:python :qpid :eclipse :soasuite :c :unix},
;;    :name "Buchbinder Parrington",
;;    :salary 106525},
;;   :score 3/4}
;;  {:result
;;   {:age 25,
;;    :skills #{:python :qpid :eclipse :c},
;;    :name "Gruffydd Hawksworth",
;;    :salary 394500},
;;   :score 3/4}
;;  {:result
;;   {:age 41,
;;    :skills #{:weblogic :c},
;;    :name "Joshua/Alexander Buston",
;;    :salary 145884},
;;   :score 3/4}
;;  {:result
;;   {:age 35,
;;    :skills #{:python :c :ruby :zeromq :objective-c},
;;    :name "Galen Carlton",
;;    :salary
;;  600348},
;;   :score 3/4}
;;  {:result
;;   {:age 35,
;;    :skills #{:c},
;;    :name "Hieronimus Burchall",
;;    :salary 439772},
;;   :score 3/4}
;;  {:result
;;   {:age 41,
;;    :skills #{:c :sql :os2},
;;    :name "Kowalski Curtis",
;;    :salary 654837},
;;   :score 3/4})



;; finally, now that all of the simple matchers have been demonstrated
;; here is how the matchers can be tied together to match across many
;; attributes, each with a relative importance

;; our client is looking for a c++ developer.
;; he is ok if we send him candidates with related skills (c# java etc)
;; the job pays a salary of 375 000 up to 425 000
;; our client also has a preference for candidates who are named
;; Alex or has Alex as part of their names

;; our request record will have three fields
;; :name :skill & :salary


(defn search-spec-maker
  "makes a matcher that searches for candidates based on name, skillset and salary point"
  [name-weight 
   salary-weight salary-tolerance
   skillset-weight skillset-tbl]

  (weighted-matcher-matcher identity identity
                            
                            (string-matcher :name :name)
                            name-weight
   
                            (gliding-scale-matcher :salary :salary
                                                   [{:x (* -1 salary-tolerance) :y 0}
                                                    {:x 0 :y 100}])
                            salary-weight
   
                            (weighted-set-matcher :skill :skills
                                                  skillset-tbl)
                            skillset-weight))

(defn find-people-on-profile
  "finds people based on the search-spec-maker, taking into account candidate's name, skillset and salary requirement"
  [name
   salary salary-tolerance
   skill]

  (->> (request candidates
                (search-spec-maker 20                  ;; weight of name match
                                   40 salary-tolerance ;; salary weight and tolerance
                                   40 c-style-skills-matrix)
                {:name name
                 :salary salary
                 :skill skill})
       sort-on-match-score
       (map #(select-keys % [:score :result]))))

;; as you can see, the best candidate we have has a
;; salary expectation at or lower than 375k
;; skills the same or related to c++
;; and has "Alex" as part of (her) name
;; another top candidate (Bellop Blackwood) has
;; a salary expectation, slightly too high
;; exactly the right skill (c++)
;; and not "Alex" as part of his name

;; (->> (find-people-on-profile "Alex" 375000 50000 #{:c++}) (take 20) pprint)
;; ({:result
;;   {:age 37,
;;    :skills #{:weblogic :c :d},
;;    :name "Alexa Baxter",
;;    :salary 256306},
;;   :score 9/10}
;;  {:result
;;   {:age 50,
;;    :skills #{:python :qpid :soasuite :weblogic :c :unix},
;;    :name "Blount Alexander",
;;    :salary 370568},
;;   :score 9/10}
;;  {:result
;;   {:age 25,
;;    :skills #{:python :emacs :windows :c++ :java},
;;    :name "Bellop Blackwood",
;;    :salary 317397},
;;   :score 4/5}
;;  {:result
;;   {:age 57,
;;    :skills #{:python :weblogic :c :unix :c++},
;;    :name "Beeman Autrey",
;;    :salary 156056},
;;   :score 4/5}
;;  {:result
;;   {:age 27,
;;    :skills #{:eclipse :clojure :emacs :windows :c++ :go},
;;    :name "Godmother Gale",
;;    :salary 297659},
;;   :score 4/5}
;;  {:result
;;   {:age 26,
;;    :skills #{:python :unix :ruby :emacs :windows :c++},
;;    :name "Guillory Gilliver",
;;    :salary 246267},
;;   :score 4/5}
;;  {:result
;;   {:age 58,
;;    :skills #{:emacs :windows :c++ :go :amqp},
;;    :name "Hargraves Maltman",
;;    :salary 187501},
;;   :score 4/5}
;;  {:result
;;   {:age 33,
;;    :skills #{:weblogic
;;  :apple :c++ :oberon},
;;    :name "Farnsworth/ Allcock",
;;    :salary 211238},
;;   :score 4/5}
;;  {:result
;;   {:age 51,
;;    :skills #{:qpid :soasuite :emacs :windows :c++ :go},
;;    :name "Flaubert Lacy",
;;    :salary 317011},
;;   :score 4/5}
;;  {:result
;;   {:age 57,
;;    :skills #{:c++},
;;    :name "Fitzwalter Lawler",
;;    :salary 180019},
;;   :score 4/5}
;;  {:result
;;   {:age 55,
;;    :skills #{:qpid :windows :c++ :amqp},
;;    :name "Geremio Bayes",
;;    :salary 330654},
;;   :score 4/5}
;;  {:result
;;   {:age 49,
;;    :skills #{:soasuite :c++ :go},
;;    :name "Brandel Hermiston",
;;    :salary 186068},
;;   :score 4/5}
;;  {:result
;;   {:age 24,
;;    :skills #{:qpid :windows :c++},
;;    :name "Bessy Chancy",
;;    :salary 152574},
;;   :score 4/5}
;;  {:result
;;   {:age 31,
;;    :skills #{:vi :pascal :windows :c++},
;;    :name "Bardo Hawtrey",
;;    :salary 349197},
;;   :score 4/5}
;;  {:result
;;   {:age 28,
;;    :skills #{:qpid :soasuite :emacs :windows :c++ :zeromq},
;;    :name "Friedman Martine",
;;    :salary 218109},
;;   :score 4/5}
;;  {:result
;;   {:age
;;  41,
;;    :skills #{:python :emacs :windows :c++},
;;    :name "Emil Chinnock",
;;    :salary 370209},
;;   :score 4/5}
;;  {:result
;;   {:age 31,
;;    :skills #{:qpid :c :c++},
;;    :name "Balfour Hillcoat",
;;    :salary 266785},
;;   :score 4/5}
;;  {:result
;;   {:age 47,
;;    :skills #{:siebel :ruby :emacs :windows :c++},
;;    :name "Betsey Bellgrove",
;;    :salary 172853},
;;   :score 4/5}
;;  {:result
;;   {:age 35,
;;    :skills #{:windows :c++ :amqp},
;;    :name "Deresco Witham",
;;    :salary 189384},
;;   :score 4/5}
;;  {:result
;;   {:age 30,
;;    :skills #{:qpid :ruby :emacs :windows :c++ :go},
;;    :name "Culloux Shriver",
;;    :salary 265543},
;;  :score 4/5})
