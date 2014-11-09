(ns fp-growth.core
  :require [clojure.zip :as zip])

;;implement the fp growth algorithm in clojure
;;to be used for basket analysis

(def tree (1 '(a b c) 2))
(def tree (zip/seq-zip (seq tree)))

;given a proper tree you should be able to
;loop through the whole bastard using zip/next
;and visit every element

(defn print-tree [tree]
  (if (zip/end? tree) 
    nil
    (do
      (if (= '(a b c) (zip/node tree))
        nil
        (println (zip/node tree)))
      (recur (zip/next tree)))))

;let's start by building a path from a list of transactions
(def trans '((a b c) (b c) (e d f) (b a c) (f e d)))
;so you'll note there are two transactions that are the same
;for all we care, the list needs to be storted so that
;the most frequent items show up as the first purchase
;this will allow the building of "prefixes" for the paths
;and allow for better compression of the data
