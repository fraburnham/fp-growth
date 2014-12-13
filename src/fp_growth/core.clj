(ns fp-growth.core
    (:import Tree ITree))

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

;;*
; Pre-sorts the data for fp-growth so that the most frequent item overall is
; placed as the first item in the list. This allows the tree building to be
; simplified.
; @param data A seq that is formatted ((item item item) (item item item))
; @param cutoff if an item occurs less times than cutoff it will be dropped
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
; @param tree-interface an interface that matches ITree
; @param tree the tree or subtree to walk
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
; use (doall (map (partial build-tree! ...) ...)). Tree building is not
; thread safe.
; @param fn-visit-node is the function to apply when a node is updated
; @param tree-interface an interface that reifys ITree
; @param tree a Tree to build up
; @param items a seq in the format (itemA itemB itemC) to add to the tree
; @return undefined the tree has already been mutated in place
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
; Returns the node's children as a list.
; @param node a Tree.Node
; @return a list of child nodes (first child added to the node is first in the
;         list.
;;*
(defn child-list [node]
  (loop [num-children (.getNumChildren node)
         ret '()]
    (if (= 0 num-children)
      ret
      (recur (dec num-children) (cons (.getChild node num-children) ret)))))

;;*
; Visits each node in a depth first walk and applies fn-visit.
; @param fn-visit! is a function that accepts a node the return value is ignored
; @param tree a Tree to walk
; @return undefined the nodes are assumed to be modified in place
;;*
(defn depth-first-visit [fn-visit! tree]
  (loop [nodes (list (.rootNode tree))]
    (if (empty? nodes)
      nil
      (let [n (first nodes)]
        (fn-visit! n)
        (recur (concat (child-list n) (rest nodes)))))))

;;*
;
; @param tree-interface an interface that reifys ITree
; @param fn-add-link takes two Tree.Nodes the first is the node we're on
;                    the second is the node to link to
; @param fn-linked? accepts a Tree.Node and returns true if a node already has
;                   a forward link
; @param tree a Tree object to find links for
; @return nil, the tree is modified in place
;;*
(defn make-links! [fn-tree-interface fn-add-link fn-linked? tree]
  (defn visit! [node]
    (if (not (fn-linked? node))
      (let [next-node (.findNode tree fn-tree-interface (.-data node) node)]
        (if (not (nil? next-node))
          (fn-add-link node next-node)))))
  (depth-first-visit visit! tree))


