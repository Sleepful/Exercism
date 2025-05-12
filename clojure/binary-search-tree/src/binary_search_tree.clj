(ns binary-search-tree)

(declare left right)

(defrecord Node [value left right])

(defn value [node]
  (:value node))

(defn singleton [value]
  (Node. value nil nil))

(defn insert
  ([new-value root-node] (insert new-value root-node [[] root-node]))
  ([new-value current-node [leaf-path root-node :as _acc]]
   (let [node-val (value current-node)]
     (cond
       (nil? node-val)
       (assoc-in root-node leaf-path (singleton new-value))

       (<= new-value node-val)
       (recur new-value (left current-node) [(conj leaf-path :left) root-node])

       :else
       (recur new-value (right current-node) [(conj leaf-path :right) root-node])))))

(defn left [node]
  (:left node))

(defn right [node]
  (:right node))

(defn to-list [root]
  (loop [current-node root
         backtrack-stack [root]
         ordered-values []
         backtrack false]
    #_(prn (:value current-node) backtrack-stack ordered-values)
    (cond
      (and (not backtrack) (:left current-node))
      (recur (:left current-node) (conj backtrack-stack (:left current-node)) ordered-values false)

      (:right current-node)
      (recur (:right current-node) (conj (pop backtrack-stack) (:right current-node)) (conj ordered-values (:value current-node)) false)

      (and current-node (seq (pop backtrack-stack))) ; leaf node, with option to backtrack
      (recur
       (peek (pop backtrack-stack))
       (pop backtrack-stack)
       (conj ordered-values (:value current-node))
       true)

      current-node ; last leaf node
      (conj ordered-values (:value current-node)))))

(defn from-list [[first & rest :as _values]]
  (reduce (fn [acc new] (insert new acc)) (singleton first) rest))
