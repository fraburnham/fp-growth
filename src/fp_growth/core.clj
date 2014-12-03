(ns fp-growth.core
    (:import Tree ITree))

;use the Tree.class to see if it offeres speed improvements over
;clojure.zip

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

;;TODO: pre-sort needs to drop all items below the cutoff to speed up
;;TODO: tree generation. The same should be done in the clojure.zip
;;TODO: implementation.
;;*
;;Pre-sorts the data for fp-growth so that the most frequent item overall is
;;placed as the first item in the list. This allows the tree building to be
;;simplified.
;;@param data A seq that is formatted ((item item item) (item item item))
;;@return sorted data, ready for tree building
;;*
(defn pre-sort [data]
  (let [freqs (apply (partial merge-with +) (map frequencies data))]
    (defn item-freq-list [x]
      (list x (x freqs)))
    (map #(map first %)
         (map #(sort-by last >
                        (alpha-by-first-char (map item-freq-list %)))
              data))))

;;*
;;An instance of the ITree interface to use for comparing and
;;printing node values. Pass when needed as (tree-interface)
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

;reduce over the pre-sorted data?
;check the rootNode for the first item, if it exists, inc the value
;  if it doesn't exist add it as a child to the rootNode of the tree
;for the next item assume that the matching node/node we just created
;  is the new "rootNode" for the "new tree". to keep items in order

;;*
;;Check if a node has a child with the given data
;;@param data-compare A function that returns true when both inputs match
;;@param root-node A Tree.Node to start the search from
;;@param child-data The value to compare against Node.data
;;@retrun A Tree.Node if one exists nil otherwise
;;*
(defn get-child [data-compare root-node child-data]
  (loop [x (.getNumChildren root-node)]
    (if (= x -1)
      nil ;got to the end and saw no matches along the way
      (let [node (.getChild root-node x)] ;get the xth child of root-node
        (if (data-compare (.-data node) child-data)
          node ;found the node
          (recur (dec x))))))) ;didn't find the node, dec x and check again

(defn build-tree! [fn-inc-node tree items]
  (loop [r (.-rootNode tree)
         items items]
    (if (empty? items)
      tree
      (let [find-child (get-child = r (first items))]
        ;need to perform a check here to see if the 
        ;child exists and update it otherwise add a new
        ;child
        (if (nil? find-child)
          ;if the child doesn't exist add it
          (.addChild r (.newNode tree (first items)))
          ;if the child does exist update the node
          (fn-inc-node find-child))
        ;now recur
        (recur (.getChild r (dec (.getNumNodes r))) (rest items))))))
