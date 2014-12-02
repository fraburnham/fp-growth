(ns fp-growth.core
    (:import Tree ITree))

;use the Tree.class to see if it offeres speed improvements over
;clojure.zip

;;TODO: pre-sort needs to drop all items below the cutoff to speed up
;;TODO: tree generation. The same should be done in the clojure.zip
;;TODO: implementation.
;;*
;;Pre-sorts the data for fp-growth so that the most frequent item overall is
;;placed as the first item in the list. This allows the tree building to be
;;simplified.
;;@param data A seq that is formatted ((item item item) (item item item))
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
;;printing node values. Pass when needed as (tree-interface.)
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

;so the basics like node adding are already written, so let's play with some tests to build a tree
;and see how it behaves
(def t (new Tree))
(.addChild (.-rootNode t) (.newNode t 1))
(.addChild (.-rootNode t) (.newNode t 2))
(.addChild (.-rootNode t) (.newNode t "Tree<Integer>"))
(.addChild (.getChild (.-rootNode t) 2) (.newNode t "SUPER CHILD type means nothing to me"))
(.printTree t (tree-interface.) (.-rootNode t))


