(ns fp-growth.core
  :require [clojure.zip :as zip])

;;implement the fp growth algorithm in clojure
;;to be used for basket analysis

;data is in the format
;((item item item) (item item item))
;a transaction is a single list of items

;I'm using closures to increase readability, is that bad practice?
(defn fp-growth-pre-sort [data]
  (let [freqs (apply (partial merge-with +) (map frequencies data))]
    (defn item-freq-list [x]
      (list x (x freqs)))
    (defn alpha-by-first-char [x]
      (sort-by (comp str first) compare x))
    (map #(map first %)
         (map #(sort-by last > 
                        (alpha-by-first-char (map item-freq-list %)))
              data))))

;path building is something like
;given (b c a)
;zip/insert-child tree b
;(with new tree)
;zip/down tree (puts us at b)
;zip/insert-child tree c
;etc
;needs to check each time if the child node already exists and just
;increment if it does
;gotta ponder how to handle the links, there may not be a super fast way to do
;them
