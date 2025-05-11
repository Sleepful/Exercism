(ns flatten-array)

(defn flatten
  "Flattens the given sequential collection.
  Nil values are excluded from the result."
  [coll]
  (filter some? (clojure.core/flatten coll)))


