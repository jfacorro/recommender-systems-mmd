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

(defn question-B-1 []
  (let [col [2/7 3/7 6/7]
        options [[-0.937, 0.312, 0.156]
                 [-0.288, -0.490, 0.772]
                 [-0.548, 0.401, 0.273]
                 [0.312, 0.156, -0.937]]]
    (clojure.pprint/pprint
      [(map magnitude options)
       (map (partial m/inner-product col) options)])))

(defn question-B-3 []
  (let [m [[1 1]
           [2 2]
           [3 4]]
        x (/ (dec (m/row-count m)))
        m (m/inner-product (m/transpose m) m)]
    (prn x)
    (prn m)
    (m/mul x m)))

(defn question-B-4 []
  (let [opts [[-2, 3, -1]
              [1, -2, 1]
              [-1, 1, -1]
              [-4, 2, -1]]
        x    [1, 2, 3]]
    (map (partial m/inner-product x) opts)))

(comment
  (question-A-1)
  (map question-A-2 [0 0.5 1 2])

  (question-B-1)
  (question-B-3))
(question-B-4)
