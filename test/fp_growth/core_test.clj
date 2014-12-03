(ns fp-growth.core-test
  (:require [clojure.test :refer :all]
            [fp-growth.core :refer :all]))

(def alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZ")

(defn random-string-builder [num-letters]
  (apply str (repeatedly num-letters #(rand-nth alphanumeric))))

(defn random-ticket [num-items]
  (repeatedly num-items #(random-string-builder 3)))

(defn map-visit [node]
  (let [data (.-data node)
        k (first (keys data))]
    (set! (.-data node) {k (inc (k data))})))

(defn map-compare [node-data data]
  (let [nk (first (keys node-data))
        dk (first (keys data))]
    (= nk dk)))

(defn mapify-items [items]
  (map (fn [item]
         {(keyword item) 1}) items))

(def data (repeatedly 3000 #(random-ticket (inc (rand-int 10)))))

(def sorted-mapifyd-data (map mapify-items (pre-sort data)))

(map (partial build-tree! map-visit map-compare tree) sorted-mapifyd-data)

;TODO: some actual testing not just REPL testing
