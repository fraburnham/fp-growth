(ns fp-growth.core
  :require [clojure.zip :as zip])

;;implement the fp growth algorithm in clojure
;;to be used for basket analysis

;replace all instances of tree with loc for
;consistency with clojure.zip

(defmacro terminal-node? [loc]
  `(nil? (zip/down ~loc)))
;if a node is non-terminal a double zip is needed to get to the "node"?

(defmacro empty-tree []
  `(zip/seq-zip (seq '())))

(defn first-child [loc]
  (if (terminal-node? loc) nil
      (zip/down loc)))

(defn print-tree [tree]
  (if (zip/end? tree) 
    nil
    (do
      (println (zip/node tree))
      (recur (zip/next tree)))))

(defn print-terminal-nodes [tree]
  (if (zip/end? tree)
    nil
    (if (terminal-node? tree)
      (do ;if zip/down is nil this is a termial node
        (println (zip/node tree))
        (recur (zip/next tree)))
      (recur (zip/next tree)))))
;should be able to use terminal-node checking to
;decide if we can add a child

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

;data is in the format
;((item item item) (item item item))
;a transaction is a single list of items
;I'm using closures to increase readability, is that bad practice?
(defn fp-growth-pre-sort [data]
  (let [freqs (apply (partial merge-with +) (map frequencies data))]
    (defn item-freq-list [x]
      (list x (x freqs)))
    (map #(map first %)
         (map #(sort-by last > 
                        (alpha-by-first-char (map item-freq-list %)))
              data))))

(defn tree-find [tree find-node]
  (cond (= (zip/node tree) find-node) tree
        (zip/end? tree) nil
        :else (recur (zip/next tree) find-node)))

;some edit magic maybe. dunno it worked on one node
;(zip/edit search-tree 
;          (fn [node]
;            (let [val (inc ((first path) (zip/node search-tree)))]
;              {(first path) val})))

;now the path
;I just noticed it looks like the tree is being made wrong
;even on the first pass with only one path to make
;(nil
; ({b 1} 
;  ({a 1})) 
; ({c 1}))
;should be
;(nil
; ({b 1}
;  ({a 1}
;   ({c 1}))))
(defn build-path [tree path]
  (if (empty? path) 
    (zip/seq-zip (zip/root tree))
    (recur
     (zip/next (zip/insert-right tree (list {(first path) 1})))
     (rest path))))


(defn next-node [loc]
  (if (terminal-node? loc) 
    (doublezip loc)
    (zip/next loc)))

(zip/append-child (-> fp-tree zip/next doublezip doublezip zip/up) {'z 1})
;first zip/next is because fp-tree was poorly formatted
;doublezip gets us to the node instead of the branch
