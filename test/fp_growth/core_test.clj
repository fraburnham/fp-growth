(ns fp-growth.core-test
  (:require [clojure.test :refer :all]
            [fp-growth.core :refer :all])
  (:import ITree))

(def alpha "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn random-string-builder [num-letters]
  (apply str (repeatedly num-letters #(rand-nth alpha))))

(defn random-ticket [num-items]
  (repeatedly num-items #(random-string-builder 2)))

(defn map-visit [node]
  (let [data (.-data node)
        k (first (keys data))]
    (.setData node {k (inc (k data))})))

(defn map-compare [node-data data]
  (= (first (keys node-data)) (first (keys data))))

(defn mapify-items [items]
  (map (fn [item]
         {(keyword item) 1}) items))

(defn prune? [cutoff node]
  (<= (.getNumChildren node) cutoff))

(defn tree-interface []
  (reify
    ITree
    (nodeDataEquals [this a b] (map-compare a b))
    (nodeDataGreater [this a b] (> a b))
    (toString [this node]
      (if (nil? node)
        "Root"
        (let [data (.-data node)]
          (if (nil? data)
            "No Data"
            (str data)))))))

(def tree (new Tree))
(def data (repeatedly 3000 #(random-ticket (inc (rand-int 3)))))
(def sorted-mapifyd-data (map mapify-items (pre-sort data 3)))
(map (partial build-tree! map-visit (tree-interface) tree) sorted-mapifyd-data)
(prune-children! (partial prune? 0) (.rootNode tree))

;TODO: some actual testing not just REPL testing
