(ns aoc.day6
  (:require [aoc.core :as c]
            [clojure.string :as str]))

(def input "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4")

(defn transfer [memory-count
                start-index
                max-amount
                index amount]
  (let [offset (- index start-index)
        circular-offset
        (if (< offset 0)
          (+ (- memory-count start-index) index)
          offset)
        amount-for-bank (max 1
          (if (= (mod max-amount memory-count) 0)
              (quot max-amount memory-count)
              (quot max-amount (dec memory-count))))]
    (cond
      (= offset 0)
      (- amount (min max-amount
                     (* amount-for-bank (dec memory-count))))
      (<= circular-offset max-amount) (+ amount amount-for-bank)
      :else amount)))

(defn redistribute [memory]
  (let [memory-count (count memory)

        [max-memory-bank-index max-memory-bank-amount]
        (last (sort-by #(vec (reverse (update % 0 (partial * -1))))
                 (map-indexed vector memory)))]

    (vec (map-indexed (partial transfer
                               memory-count
                               max-memory-bank-index
                               max-memory-bank-amount)
                      memory))))

(defn redistribution [memory] (iterate redistribute memory))

(defn parse-memory [txt]
  (vec (map c/parse-int (str/split txt #"\t"))))

(defn loop-detection [memory]
  (let [r (redistribution memory)]
    (dec (c/find-first #(apply (complement distinct?)
                               (take % r))
                (drop 1 (range))))))

;; Result - part 1
; (loop-detection (parse-memory input))
; => 11137

;; Result - part 2
; (nth (redistribution (parse-memory input)) 11138)
; (c/find-first #(= (second %) [0 14 13 12 10 9 9 7 7 5 5 4 2 2 1 12])
;   (map-indexed vector (redistribution (parse-memory input))))
; (- 11138 10101)
; => 1037