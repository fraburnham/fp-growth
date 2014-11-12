(ns fp-growth.core
  :require [clojure.zip :as zip])

(defmacro empty-tree []
  `(zip/seq-zip (seq '())))

(defn next-subbranch [loc]
  (zip/right (zip/down loc)))

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

(defn next-branch [loc]
  (let [nloc (zip/next loc)]
    (if (zip/branch? nloc) 
      nloc
      (recur nloc))))

(defn add-branch [loc]
  (zip/edit loc 
            (fn [node] 
              (if (empty? (rest node))
                (conj '(()) (first node))
                (conj (conj (rest node) '()) (first node))))))

;data is in the format
;((item item item) (item item item))
(defn pre-sort [data]
  (let [freqs (apply (partial merge-with +) (map frequencies data))]
    (defn item-freq-list [x]
      (list x (x freqs)))
    (map #(map first %)
         (map #(sort-by last > 
                        (alpha-by-first-char (map item-freq-list %)))
              data))))

(defn find-node-in-branch-this-depth [loc find-key]
  (if (nil? loc) 
    nil
    (let [node (zip/node (zip/down loc))
          k (if (= (type node) (type {}))
              (first (keys node))
              nil)]
      (if (= k find-key) 
        loc
        (recur (zip/right loc) find-key)))))

;inc the val in the dict that the node holds
(defn update-node [loc]
  (zip/edit 
   loc 
   (fn [node]
     (let [m (first node)
           k (first (keys m))]
       (cons {k (inc (k m))} (rest node))))))

;takes an (empty-tree) as loc
;or an exsisting tree to add to
(defn build-path [loc path]
  (if (empty? path)
    (zip/seq-zip (zip/root loc))
    (let [new-node {(first path) 1}
          find-key (first path)
          nn (if (= (next-subbranch loc) nil) nil 
                 (find-node-in-branch-this-depth (next-subbranch loc) 
                                                 find-key))]
      (if (= nn nil)
        (do
          (recur
           (zip/append-child (next-branch (add-branch loc)) new-node)
           (rest path)))
        (recur
         (update-node nn)
         (rest path))))))

;next the tree needs to be pruned
;search for paths/nodes that have only 1 purchase
;remove them
;then the links between like items can be made

(defn prune-tree [loc cutoff]
  (if (zip/end? loc)
    (zip/seq-zip (zip/root loc))
    (let [node (zip/node loc)]
      (if (= (type node) (type {}))
        (let [k (first (keys node))]
          (if (< (k node) cutoff)
            (recur (zip/remove (zip/up loc)) cutoff)
            (recur (zip/next loc) cutoff)))
        (recur (zip/next loc) cutoff)))))

;some cleanup for the adam and eve dataset
;(def smallsample (pre-sort (map #(filter (comp not nil?) %) (take 5 (drop 3 item-titles)))))

