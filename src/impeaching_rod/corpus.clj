(ns impeaching-rod.corpus
  "A corpus is a collection of documents (results) and queries (requests)"
  (:use [impeaching-rod.common]
        [clojure.set]))

(def BATCH-SIZE
  "This is the batch size as used by request. The documents in a corpus will be
split into separate collections of this size and the request will be run concurrently
on these separate document collections."
  1000)

(defn create-corpus [name]
  "Creates a corpus with a specific name and returns it. This function does not have a side-effect.

The corpus consists of:
- a name
- a _set_ of documents. Adding a document to the corpus that is there already is a no-op
"
  {:type :corpus
   :name name
   :documents #{}})

(defn add-document [corpus doc]
  "Adds document doc to the corpus list of documents."
  (assoc corpus
    :documents
    (conj (:documents corpus)
          doc)))

(defn remove-document-where [corpus p]
  "Removes documents where predicate p is true"
  (let [d (:documents corpus)]
    (assoc corpus
      :documents
      (difference d (set (filter p d))))))
    
(defn remove-document [corpus doc]
  "removes a specific item from the document set"
  (assoc corpus
    :documents
    (difference (:documents corpus) #{doc})))
  
(defn request [corpus matcher request]
  "Returns a lazy sequence of result documents along with the scores.
This function makes use of pmap, make sure to call (shutdown-agents) when
the app must terminate early"

  (letfn [(match-document [res]
            {:result res
             :request request
             :score (matcher request res)})
          (match-seq [seq]
            (map #(match-document request %) seq))]
    (->> (:documents corpus)
         (partition BATCH-SIZE BATCH-SIZE nil)
         (pmap match-seq)
         flatten)))
