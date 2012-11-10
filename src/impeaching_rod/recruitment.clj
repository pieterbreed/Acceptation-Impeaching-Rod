(ns impeaching-rod.recruitment
  (:use impeaching-rod.common
        impeaching-rod.rules
        impeaching-rod.corpus
        impeaching-rod.fake-data))

;; creates a corpus with 10 000 candidate resumes
(def candidates (let [c (create-corpus "candidate db")]
                  (add-documents c (take 10000 (make-people)))
                  c))

;; request is defined in the corpus package
;; it takes the corpus itself, a matcher and a request record
;; find-everyone-of-age also filter based on the matching value
;; in this case 1 means full match and 0 means no match at all
;; which is what you want for an age equality matcher
;; then lastly it maps the corpus document (the thing you searched for)
;; out of the match-result record
;; match result records contains the request document, the corpus doc
;; and the score

(defn find-everyone-of-age
  "finds all the candidates that has a specific age in the candidates corpus"
  [age]
  (->> (request candidates
                (simple-matcher identity ;; means the request record (here, age)
                                         ;; is it's own value
                                :age)    ;; means use the age value from the result record
                age)
       (filter #(= 1 (:score %)))
       (map :result)))

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

(defn find-everyone-with-name-part
  "finds all candidates that has 'name' as part of their name"
  [name]
  (->> (request candidates
                (string-matcher identity
                                :name) ;; means match against name field in corpus doc
                name)
       (filter #(= 1 (:score %)))
       (map :result)))

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

(defn find-everyone-of-age-between
  "finds everyone between the ages of a and b"
  [a b]
  (->> (request candidates
                (range-matcher identity
                               :age)
                {:start a :end b})
       (filter #(= 1 (:score %)))
       (map :result)))

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

(defn sort-on-match-score
  "sorts match result documents based on the match score, highest to lowest"
  [results]
  (->> results
       (sort-by (fn [r] (:score r)))
       reverse))

(defn find-and-rank-people-with-skills
  "Specifies a set of skills and ranks candidates based on how closely their skillset matches the required skillset"
  [skillset]
  {:pre [(set? skillset)]} ;; pre-condition, skillset must be a set
  (->> (request candidates
                (set-matcher identity
                             :skills)
                skillset)
       sort-on-match-score
       (map #(select-keys % [:score :result]))))

;; eg demonstrating pre-condition failure
;; (take 10 (find-and-rank-people-with-skills {:test 0})))
;; AssertionError Assert failed: (set? skillset)  impeaching-rod.recruitment/find-and-rank-people-with-skills (recruitment.clj:119)

;; (find-and-rank-people-with-skills #{:zeromq :python :unix :go}))
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
;; increasing

;; eg (take 10 (reverse (find-people-who-will-be-happy-with-salary 695000 5000)))
;; ({:result
;;   {:age 55,
;;    :skills #{:c :javascript},
;;    :name "Lightoller Cobb",
;;    :salary 699997},
;;   :score 3/50}
;;  {:result
;;   {:age 49, :skills #{}, :name "Abra Bloodworth", :salary 699995},
;;   :score 1/10}
;;  {:result
;;   {:age 54,
;;    :skills #{:python :weblogic},
;;    :name "Windt Fallowfield",
;;    :salary 699897},
;;   :score 103/50}
;;  {:result
;;   {:age 21, :skills #{}, :name "Linus Marquis", :salary 699886},
;;   :score 57/25}
;;  {:result
;;   {:age 40,
;;    :skills #{:weblogic :vi :pascal :emacs :windows},
;;    :name "Czarevitch Cockburn",
;;    :salary 699668},
;;   :score 166/25}
;;  {:result
;;   {:age 55,
;;    :skills #{:python :qpid :soasuite :c},
;;    :name "Bruckner Philipson",
;;    :salary 699623},
;;   :score 377/50}
;;  {:result
;;   {:age 25,
;;    :skills #{:python :weblogic},
;;    :name "Blunder Gefvert",
;;    :salary 699531},
;;   :score 469/50}
;;  {:result
;;   {:age 21,
;;    :skills #{:emacs :windows :go},
;;    :name "Army Sampson",
;;    :salary 699528},
;;   :score 236/25}
;;  {:result
;;   {:age 46, :skills #{}, :name "Arta
;; gnan Lancaster", :salary 699448},
;;   :score 276/25}
;;  {:result
;;   {:age 26,
;;    :skills #{:eclipse :emacs :c++},
;;    :name "Padua Ingram",
;;    :salary 699399},
;;   :score 601/50})

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



  
                


       