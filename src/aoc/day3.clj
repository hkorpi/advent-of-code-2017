(ns aoc.day3
  (:require [aoc.core :as c]))

(defn next-location-coordinate [[x y]]
  (cond
    (>= (- y) (c/abs x)) [(inc x), y]
    (>= (- x) (c/abs y)) [x, (dec y)]
    (>= y (c/abs x))     [(dec x), y]
    (>= x (c/abs y))     [x, (inc y)]))

(def spiral-memory-coordinates (iterate next-location-coordinate [0 0]))

(defn distance [location]
  (let [[x y] (nth spiral-memory-coordinates (dec location))]
    (+ (c/abs x) (c/abs y))))

;; Result - part 1
; (distance 265149)
; => 438

(defn neighbours [coordinate]
  (map #(c/coll+ coordinate %)
       [[-1  1] [0  1] [1 1]
        [-1  0]        [1 0]
        [-1 -1] [0 -1] [1 -1]]))

(defn location-value [coordinate memory]
  (let [neighbour-values (map #(or (get memory %) 0)
                              (neighbours coordinate))]
    (reduce + 0 neighbour-values)))

(defn next-location-value [{coordinate :coordinate
                            value :value
                            memory :memory}]
  (let [next-coordinate (next-location-coordinate coordinate)
        next-value (location-value next-coordinate memory)]
    {:coordinate next-coordinate
     :value next-value
     :memory (assoc memory next-coordinate next-value)}))

(def allocated-spiral-memory
  (iterate next-location-value
           {:coordinate [0 0]
            :value 1
            :memory (assoc {} [0 0] 1)}))

;; Result - part 2
; (c/find-first (fn [{value :value}] (>= value 265149)) allocated-spiral-memory)
; => { :coordinate [3 4], :value 266330 }