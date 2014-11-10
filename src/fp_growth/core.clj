(ns fp-growth.core
  :require [clojure.zip :as zip])

;;implement the fp growth algorithm in clojure
;;to be used for basket analysis

(defmacro terminal-node? [loc]
  `(nil? (zip/down ~loc)))

(defmacro empty-tree []
  `(zip/seq-zip (seq '())))

(defn first-child [loc]
  (if (terminal-node? loc) nil
      (zip/down loc)))

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

(defn tree-find [loc find-node]
  (cond (= (zip/node loc) find-node) loc
        (zip/end? loc) nil
        :else (recur (zip/next loc) find-node)))

(defn tree-find-key-in-map [loc find-key]
  (let [node (zip/node loc)
        k (if (= (type node) (type {})) 
            (first (keys node))
            nil)]
    (cond (= k find-key) loc
          (zip/end? loc) nil
          :else (recur (zip/next loc) find-key))))

(defn next-node [loc]
  (let [nloc (zip/next loc)]
    (if (zip/branch? nloc)
      (recur nloc)
      nloc)))

(defn next-branch [loc]
  (let [nloc (zip/next loc)]
    (if (zip/branch? nloc) 
      nloc
      (recur nloc))))

(defn add-branch [loc]
  (zip/edit loc (fn [[node r]] 
                  (if (nil? r)
                    (conj '(()) node)
                    (conj (list () r) node)))))

;inc the val in the dict that the node holds
(defn update-node [loc]
  (zip/edit 
   loc 
   (fn [node]
     (let [k (first (keys node))]
       {k (inc (k node))}))))

;takes an (empty-tree) as loc
;or an exsisting tree to add to
(defn build-path [loc path]
  (let [new-node {(first path) 1}]
    (if (empty? path) 
      (zip/seq-zip (zip/root loc))
      (let [find-key (first path)
            nn (tree-find-key-in-map loc find-key)]
        (if (= nn nil)
          (recur
            (zip/append-child (next-branch (add-branch loc)) new-node)
            (rest path))
          (recur
           (update-node nn)
           (rest path)))))))

;now all that is left is to build the links between branches for items
