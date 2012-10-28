(ns impeaching-rod.corpus
  "A corpus is a collection of documents (results) and queries (requests)"
  (:use [impeaching-rod.common]))

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
    
  
  

