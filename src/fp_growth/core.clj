(ns fp-growth.core
    (:import Tree ITree))

;use the Tree.class to see if it offeres speed improvements over
;clojure.zip. In super preliminary tests it is quicker

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

;;*
; Pre-sorts the data for fp-growth so that the most frequent item overall is
; placed as the first item in the list. This allows the tree building to be
; simplified.
; @param data A seq that is formatted ((item item item) (item item item))
; @return sorted data, ready for tree building
;;*
(defn pre-sort [data cutoff]
  (let [freqs (apply (partial merge-with +)
                     (map frequencies (map #(map keyword %) data)))]
    (defn item-freq-list [x]
      (list x ((keyword x) freqs)))
    (remove empty?
            (map (fn [x]
                   (remove nil?
                           (map first
                                (map (fn [y] (if (>= (last y) cutoff) y)) x))))
                 (map #(sort-by last >
                                (alpha-by-first-char (map item-freq-list %)))
                      data)))))

;;*
; An instance of the ITree interface to use for comparing and
; printing node values. Pass when needed as (tree-interface)
;;*
(defn tree-interface []
  (reify
    ITree
    (nodeDataEquals [this a b] (= a b))
    (nodeDataGreater [this a b] (> a b))
    (toString [this node]
      (if (nil? node)
        "Root"
        (let [data (.-data node)]
          (if (nil? data)
            "No Data"
            (str data)))))))

;;*
; Check if a node has a direct child with the given data
; @param data-compare A function that returns true when both inputs match
; @param root-node A Tree.Node to start the search from
; @param child-data The value to compare against Node.data
; @retrun A Tree.Node if one exists nil otherwise
;;*
(defn get-child [data-compare root-node child-data]
  (loop [x (dec (.getNumChildren root-node))]
    (if (= x -1)
      nil ;got to the end and saw no matches along the way
      (let [node (.getChild root-node x)] ;get the xth child of root-node
        (if (data-compare (.-data node) child-data)
          node ;found the node
          (recur (dec x))))))) ;didn't find the node, dec x and check again

;;*
; Builds the tree (in place) one transaction at a time. Update nodes
; if they already exist, create them otherwise. Sticks to a single path.
; To build a tree from a list like ((item item item) (item item item)),
; use (map (partial build-tree! ...) ...)
; @param fn-visit-node is the function to apply when a node is updated
; @param fn-compare-data is used to compare the current item with the child
;        nodes' data.
; @param tree a Tree to build up
; @param items a seq in the format (itemA itemB itemC) to add to the tree
; @return nil the tree has already been mutated in place
;;*
(defn build-tree! [fn-visit-node fn-compare-data tree items]
  (loop [r (.-rootNode tree)
         items items]
    (if (empty? items)
      nil
      (let [find-child (get-child fn-compare-data r (first items))]
        (if (nil? find-child)
          ;if the child doesn't exist add it
          (do
            (.addChild r (.newNode tree (first items)))
            (recur (.getChild r (dec (.getNumChildren r))) (rest items)))
          ;if the child does exist update the node
          (do
            (fn-visit-node find-child)
            (recur find-child (rest items))))))))

;;*
; Prunes a node's children (in place) based on the return value of fn-prune?.
; @param fn-prune? accepts a node
;                  returns true if a node should be pruned, otherwise false
; @param root-node the node to start pruning from
; @return undefined (return value is un-needed as node is modified in place)
;;*
(defn prune-children! [fn-prune? root-node]
  ;apply fn-prune? to each child-node to determine if it should be pruned
  (loop [x (dec (.getNumChildren root-node))]
    (if (= x -1)
        nil
        (do
          (if (fn-prune? (.getChild root-node x))
            (.removeChild root-node x))
          (recur (dec x))))))
