(ns fp-growth.core
  :require [clojure.zip :as zip])

;;implement the fp growth algorithm in clojure
;;to be used for basket analysis

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

;needs to check each time if the child node already exists and just
;increment if it does
;gotta ponder how to handle the links, there may not be a super fast way to do
;them

(def first-path '(b c a))
(def blank-tree (zip/seq-zip (seq '(nil))))

(defmacro empty-tree []
  `(zip/seq-zip (seq '(nil))))

;now the path
(defn build-path [tree path]
  (if (empty? path) (zip/seq-zip (zip/root tree))
      (recur
       (if (= (first (zip/node tree)) (first path))
         ;do edit and recur
         (zip/next (zip/insert-child tree (list (first path) 1))))
       (rest path))))
    
