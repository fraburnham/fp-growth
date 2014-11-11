(ns fp-growth.core
  :require [clojure.zip :as zip])

(defmacro terminal-node? [loc]
  `(nil? (zip/down ~loc)))

(defmacro empty-tree []
  `(zip/seq-zip (seq '())))

(defn first-child [loc]
  (if (terminal-node? loc) nil
      (zip/down loc)))

;requires that a branch is passed
(defn next-subbranch [loc]
  (zip/right (zip/down loc)))

(defn alpha-by-first-char [x]
  (sort-by (comp str first) compare x))

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
  (zip/edit loc 
            (fn [[node r]] 
              (if (nil? r)
                (conj '(()) node)
                (conj (list () r) node)))))

;data is in the format
;((item item item) (item item item))
;a transaction is a single list of items
;I'm using closures to increase readability, is that bad practice?
(defn pre-sort [data]
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

(defn tree-find-key-in-map-subbranch [loc find-key]
  (println "SEARCHING" loc)
  ;the if below may be superflous
  (if (not (nil? (zip/right loc)))
    (tree-find-key-in-map-subbranch ((zip/right loc) find-key)))
  (if (nil? loc) ;hit the bottom of this branch, don't look into the next one
    nil
    (let [node (do (println (zip/node (zip/down loc))) 
                   (zip/node (zip/down loc)))
          k (if (= (type node) (type {}))
              (first (keys node))
              nil)]
      (println k node)
      (cond (= k find-key) loc ;return the loc where the key is
            (zip/end? loc) nil ;this may never happen ...
            :else (recur (next-subbranch loc) find-key)))))

;i may be searching way to hardcore with that algo
;also it's broken at the end

(defn find-node-in-branch-this-depth [loc find-key]
  (println loc)
  (if (nil? loc) 
    nil
    (let [node (zip/node (zip/down loc))
          k (if (= (type node) (type {}))
              (first (keys node))
              nil)]
      (println node k)
      (if (= k find-key) 
        loc
        (recur (zip/right loc) find-key)))))

;inc the val in the dict that the node holds
(defn update-node [loc]
  (println "UPDATING")
  (zip/edit 
   loc 
   (fn [node]
     (let [m (first node)
           k (first (keys m))]
       (cons {k (inc (k m))} (rest node))))))

;takes an (empty-tree) as loc
;or an exsisting tree to add to
(defn build-path [loc path]
  (let [new-node {(first path) 1}]
    (if (empty? path) ;this if could be higher up need to refactor
      (zip/seq-zip (zip/root loc))
      (let [find-key (first path)
            nn (if (= (next-subbranch loc) nil) nil
                   (find-node-in-branch-this-depth (next-subbranch loc) 
                                                   find-key))]
        (if (= nn nil)
          (recur
            (zip/append-child (next-branch (add-branch loc)) new-node)
            (rest path))
          (recur
           (update-node nn)
           (rest path)))))))
