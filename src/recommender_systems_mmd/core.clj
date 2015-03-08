(ns recommender-systems-mmd.core
  (:require [clojure.core.matrix :as m]
            [clojure.core.matrix.operators :as mop]))

(defn avg [v]
  (-> (reduce + v)
    (/ (m/row-count v))))

(defn row-avgs [m]
  (map avg (m/rows m)))

(defn col-avgs [m]
  (map avg (m/columns m)))

(defn magnitude [x]
  (m/distance x (m/zero-vector (m/row-count x))))

(defn cos-distance [x y]
  (-> (m/inner-product [1 1] [1 1])
    (/ (* (magnitude x)
         (magnitude y)))))

(defn question-A-1 []
  (let [m [[1 2 3 4 5]
           [2 3 2 5 3]
           [5 5 5 3 2]]
        ravgs (row-avgs m)
        mprime (->> m
                 m/rows
                 (map #(map (fn [x] (- x %1)) %2) ravgs))
        cavgs (col-avgs mprime)]
    (->> mprime
      m/columns
      (map #(map (fn [x] (- x %1)) %2) cavgs)
      m/transpose)))

(defn question-A-2 [alpha]
  (let [m     [[1 0 1 0 1 (* alpha 2)]
               [1 1 0 0 1 (* alpha 6)]
               [0 1 0 1 0 (* alpha 2)]]
        letters {0 :A
                 1 :B
                 2 :C}
        pairs (for [i (range 3) j (range 3) :when (< i j)]
                [i j])
        f     (fn [[i j]]
                (cos-distance
                  (m/get-row m i)
                  (m/get-row m j)))
        g     (fn [pos]
                (mapv letters pos)) 
        result (map (juxt g f) pairs)]
    (prn :alpha alpha)
    (clojure.pprint/pprint result)))

(question-A-1)
(map question-A-2 [0 0.5 1 2])
