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

;GOTTA ZIP NEXT OFF THE ROOT NODE
(defn keep-branches-with-children [loc]
  (if (nil? (zip/right loc)) ;this almost works unless the last branch has
                             ;no children
    (zip/seq-zip (zip/root loc))
    (if (zip/branch? loc)
      (let [children (next-subbranch loc)]
        (println loc children)
        (if (nil? children)
          (recur (zip/next (zip/remove loc)))
          (recur (zip/right loc))))
      (recur (zip/right loc)))))

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

(defn link-node [loc nextloc]
  (zip/edit
   loc
   (fn [node]
     (let [node-key (first (keys node))
           remap-node-first 
            (fn [m]
              (into 
               (sorted-map-by
                (fn [x y] (if (= x node-key) -1 +1))) m))
           newm (remap-node-first (assoc node :link nextloc))]
       newm))))

;not right this instant, but this feels like it could be a reduce
(defn item-link-tree [loc items]
  (println items)
  (if (empty? items)
    (zip/seq-zip (zip/root loc))
    ;AHA! this needs to be a loop innit until we run
    ;out of next matches
    (let [item (first items)
          matchloc (tree-find-key-in-map loc item)
          nextmloc (tree-find-key-in-map (zip/next matchloc) item)]
      (if (nil? nextmloc)
        (recur (zip/seq-zip (zip/root loc)) (rest items))
        (do
          (println "Linking")
          (recur (zip/next (link-node matchloc nextmloc)) items))))))

;so the code seems to be working fp-growth ftw
;test against some larger datasets and see how life goes
