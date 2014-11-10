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

(defn print-tree [loc]
  (if (zip/end? loc 
    nil
    (do
      (println (zip/node loc))
      (recur (zip/next loc)))))

(defn print-terminal-nodes [loc]
  (if (zip/end? loc)
    nil
    (if (terminal-node? loc)
      (do
        (println (zip/node loc))
        (recur (zip/next loc)))
      (recur (zip/next loc)))))

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
  (zip/edit loc (fn [[node _]] (conj '(()) node))))

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
      ;sweet, so the concept is working, but the search is weak
      ;it'll need to look IN the dict to see if the keys match
      ;work on that there and then the first two thirds of fp-growth
      ;are ready
      (let [next-node (tree-find loc new-node)]
        (println (zip/node next-node) new-node)
        (if (= (zip/node next-node) new-node)
          (recur
           (update-node next-node)
           (rest path))
          (recur
            (zip/append-child (next-branch (add-branch loc)) new-node)
            (rest path)))))))

;ok, I'm getting it branch->node->branch->node
;nodes hold our data
;and branches describe where the nodes are
