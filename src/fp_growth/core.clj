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

;! I only want to do this at the first level. at some level
;no one has children idiot
;zip/down your root node before starting
;zip/right sets loc to nil and not to the loc on a tree
(defn keep-branches-with-children [loc]
  (if (nil? (zip/right loc)) ;this almost works unless the last branch has
                             ;no children
    (zip/seq-zip (zip/root loc))
    (if (zip/branch? loc)
      (let [children (next-subbranch loc)]
        (if (nil? children)
          (recur (zip/next (zip/remove loc)))
          (recur (zip/right loc))))
      (recur (zip/right loc)))))

;now to link the like items

;some cleanup for the adam and eve dataset
;(def smallsample (pre-sort (map #(filter (comp not nil?) %) (take 5 (drop 3 item-titles)))))
