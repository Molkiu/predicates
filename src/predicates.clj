(ns predicates
  (:require [clojure.math.numeric-tower :as math]))

(defn sum-f [f g x]
  (+ (f x) (g x)))

(defn less-than [n]
  (fn [k] (< k n)))

(defn equal-to [n]
  (fn [k] (== k n)))

(defn set->predicate [a-set]
  (fn[x](contains? a-set x)))

(defn pred-and [pred1 pred2]
  (fn [x](and
           (pred1 x)
           (pred2 x))))

(defn pred-or [pred1 pred2]
  (fn [x](or
           (pred1 x)
           (pred2 x))))

(defn whitespace? [character]
  (Character/isWhitespace character))

(defn blank? [string]
  (every? whitespace? string))

(defn has-award? [book award]
  (cond
    (:awards book) (contains? (:awards book) award)
    :else false))

(defn HAS-ALL-THE-AWARDS? [book awards]
  (let [number-of-awards (:awards book)]
    (every? identity (map (set->predicate number-of-awards) awards))))

(defn my-some [pred a-seq]
  (cond
    (empty? a-seq) false
    (pred (first a-seq)) (pred (first a-seq))
    :else (my-some pred (rest a-seq))))

(defn my-every? [pred a-seq]
  (cond
    (empty? a-seq) true
    (not(pred (first a-seq))) false
    :else (my-every? pred (rest a-seq))))

(defn prime? [n]
  (let [divisor? (fn recur[x]
                   (cond
                     (< x 2) true
                     (== (mod n x) 0) false
                     :else (recur (- x 1))))
        m (int(math/sqrt n))]
    (divisor? m)))
;^^
