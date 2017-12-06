(ns aoc.day6
  (:require [aoc.core :as c]
            [clojure.string :as str]))

(def input "14\t0\t15\t12\t11\t11\t3\t5\t1\t6\t8\t4\t9\t1\t8\t4")

(defn transfer [memory-count
                start-index
                max-amount,
                transfer-amount
                index amount]
  (let [offset (- index start-index)
        circular-offset
        (if (< offset 0)
          (+ (- memory-count start-index) index)
          offset)]
    (cond
      (= offset 0)
      (- amount (min max-amount
                     (* transfer-amount (dec memory-count))))
      (<= circular-offset max-amount) (+ amount transfer-amount)
      :else amount)))

(defn redistribute [memory]
  (let [memory-count (count memory)

        [max-memory-bank-index max-memory-bank-amount]
        (last (sort-by #(vec (reverse (update % 0 (partial * -1))))
                 (map-indexed vector memory)))

        transfer-amount
        (max 1
          (if (= (mod max-memory-bank-amount memory-count) 0)
            (quot max-memory-bank-amount memory-count)
            (quot max-memory-bank-amount (dec memory-count))))]

    (vec (map-indexed (partial transfer
                               memory-count
                               max-memory-bank-index
                               max-memory-bank-amount,
                               transfer-amount)
                      memory))))

(defn redistribution [memory] (iterate redistribute memory))

(defn parse-memory [txt]
  (vec (map c/parse-int (str/split txt #"\t"))))

(defn loop-detection [memory]
  (let [r (redistribution memory)]
    (reduce (fn [past memory]
              (if (contains? past memory)
                (reduced (count past))
                (conj past memory))) #{} r)))

;; Result - part 1
; (loop-detection (parse-memory input))
; => 11137

;; Result - part 2
; (let [memory-loop (nth (redistribution (parse-memory input)) 11138)]
;   (c/find-first
;     #(= (second %) memory-loop)
;     (map-indexed vector (redistribution (parse-memory input)))))
; (- 11138 10101)
; => 1037