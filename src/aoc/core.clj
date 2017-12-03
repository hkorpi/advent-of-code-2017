(ns aoc.core
  (:require [clojure.string :as str])
  (:import (java.util.regex Pattern)))

(defn parse-int [txt] (Integer/parseInt (str txt)))

(defn parse-csv
  [^Pattern line-separator
   ^Pattern column-separator
   ^CharSequence txt]
  (let [rows (str/split txt line-separator)]
    (map #(str/split % column-separator) rows)))

(defn cartesian [seq1 seq2]
  (for [x seq1
        y seq2]
    (list x y)))

(defn abs [number] (if (neg? number) (- number) number))

(defn find-first [predicate collection]
  (first (filter predicate collection)))

(defn coll+ [& colls]
  (apply map + colls))
