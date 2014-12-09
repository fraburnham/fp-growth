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
; Check if a node has a direct child with the given data

; @param root-node A Tree.Node to start the search from
; @param child-data The value to compare against Node.data
; @retrun A Tree.Node if one exists nil otherwise
;;*
(defn get-child [tree-interface tree root-node child-data]
  (.findNode tree tree-interface child-data root-node 1))

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
(defn build-tree! [fn-visit-node tree-interface tree items]
  (loop [r (.-rootNode tree)
         items items]
    (if (empty? items)
      nil
      (let [find-child (get-child tree-interface tree r (first items))]
        (if (nil? find-child)
          (do
            (.addChild r (.newNode tree (first items)))
            (recur (.getChild r (dec (.getNumChildren r))) (rest items)))
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

;;*
; Uses fn-update-node to create links between nodes based on
; fn-data-compare.
; @param fn-data-compare takes data from two nodes and returns
;                        true if the nodes should be considered equal
; @param fn-update-node takes a node and updates the data in place
; @param tree a Tree object to find links in
; @return nil, the tree is modified in place
;;*
(defn make-links! [fn-data-compare fn-update-node tree]
  ;see what node we're on in a depth first walk of the tree
  ;find the next occurance of the node in the tree and update the current
  ;node to include a link forward to the next node
