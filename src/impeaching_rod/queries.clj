(ns impeaching-rod.queries
  (:use impeaching-rod.common))

(defrecord Query [zef]
  clojure.lang.IFn
  (invoke [_] (println zef))
  (invoke [_ _] (println "oeu"))
  (applyTo [this args] (clojure.lang.AFn/applyToHelper this args)))
    