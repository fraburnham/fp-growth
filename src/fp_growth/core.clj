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

;this may be the wrong idea
;maybe this can be done after linking
;or pass the build path only items
;that show up in the data only once.
(defn keep-frequent-nodes [loc cutoff]
  (if (zip/end? loc)
    (zip/seq-zip (zip/root loc))
    (let [node (zip/node loc)]
      (if (= (type node) (type {})) 
       (let [k (first (keys node))]
          (if (< (k node) cutoff)
            (recur (zip/remove (zip/up loc)) cutoff)
            (recur (zip/next loc) cutoff)))
        (recur (zip/next loc) cutoff)))))

(defn keep-branches-with-children [loc]
  (cond
    (zip/end? loc) (zip/seq-zip (zip/root loc))
    (and
      (nil? (zip/down loc))
      (= 1 (count (zip/node (zip/up loc))))) (recur (zip/next (zip/remove (zip/up loc))))
    :else (recur (zip/next loc))))

;get a list of nodes that appear more than once in the tree
;the function does not consider the value of the node, only the
;keyword
(defn get-list-of-frequent-nodes [loc]
  (loop [l loc
         nodes '()]
    (if (zip/end? l) 
      (take-nth 2 (flatten (filter (fn [[key val]]
                                     (> val 1)) (frequencies nodes))))
      (if (zip/branch? l)
        (recur (zip/next l) nodes)
        (recur (zip/next l) (conj nodes (first (keys (zip/node l)))))))))

(defn tree-find-key-in-map [loc find]
  (if (zip/end? loc)
    nil
    (if (zip/branch? loc)
      (recur (zip/next loc) find)
      (if (= (type (zip/node loc)) (type {}))
        (let [node (zip/node loc)
              k (first (keys node))]
          (if (= k find)
            loc
            (recur (zip/next loc) find)))
        (recur (zip/next loc) find)))))

;returns all nodes in depth first order
(defn find-all-nodes [loc find]
  (loop [l loc
         r '()]
    (let [nloc (tree-find-key-in-map l find)]
      (if (nil? nloc)
        r
        (recur (zip/next nloc) (concat r (list nloc)))))))

(defn find-all-links [loc freq-nodes]
  (reduce merge (map #(hash-map % (find-all-nodes loc %)) freq-nodes)))

;now for a full-flow function from getting data to popping out a tree and links
(defn gen-fp-tree [data support]
  (let [tree (keep-branches-with-children
               (keep-frequent-nodes
                 (reduce build-path (cons (empty-tree) (pre-sort data))) support))
        links (find-all-links tree (get-list-of-frequent-nodes tree))]
    (list tree links)))

;TODO: this lib needs lots of if cleanup. use the clearer and more concise cond when dealing
;TODO: with multi/nested if statements