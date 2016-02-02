(ns csp.dining-philosophers.animation
  (:require [clojure.core.match :refer [match]]
            [quil.core :refer :all]
            [csp.dining-philosophers.processes :refer :all]))

(defn update-state! [ks v]
  (swap! (state-atom) assoc-in ks v))

(defn log-to-state! [& args]
  (match (vec args)
    [:room-occupancy n]              (println "Room occupancy is now" n)
    [:philosopher i :entered]        (update-state! [:philosophers i] :table)
    [:philosopher i :exited]         (update-state! [:philosophers i] :outside)
    [:fork i :picked-up-by phil]     (update-state! [:forks i] (if (= i phil) :left-held :right-held))
    [:fork i :put-down-by _]         (update-state! [:forks i] :lying)
    [:philosopher i :started-eating] (update-state! [:plates i] :used)
    [:philosopher i :stopped-eating] (update-state! [:plates i] :not-used)))

(defn setup! []
  (frame-rate 30)
  (rect-mode :center)
  (set-state!
    :philosophers (vec (repeat population :outside))
    :forks        (vec (repeat population :lying))
    :plates       (vec (repeat population :not-used)))
  (start! log-to-state!))

(defn draw-table! []
  (fill 230 240 180)
  (ellipse 0 0 200 200))

(defn draw-plate! [status]
  (case status :used (fill 50 200 100) :not-used (fill 255))
  (ellipse 0 70 45 45))

(defn draw-fork! [status]
  (if (= :lying status) (fill 255) (fill 255 0 0))
  (let [x (case status :lying 0 :left-held -28 :right-held 28)
        y (if (= :lying status) 75 85)]
    (rect x y 4 35)))

(defn draw-philosopher! [location]
  (with-translation [0 (case location :table 120 :outside 170)]
    (fill 50 130 150)
    (rect -30 -15 10 30)
    (rect 30 -15 10 30)
    (rect 0 0 70 20)
    (fill 10 80 100)
    (ellipse 0 0 30 30)))

(defn draw! []
  (background 255)
  (with-translation [(/ (width) 2) (/ (height) 2)]
    (draw-table!)
    (let [{:keys [philosophers forks plates]} (state)]
      (doseq [i (range population)]
        (with-rotation [(* (/ i population) TWO-PI)]
          (draw-plate! (plates i))
          (draw-philosopher! (philosophers i)))
        (with-rotation [(* (/ (- i 0.5) population) TWO-PI)]
          (draw-fork! (forks i)))))))

(defsketch dpsketch
           :title "Dining Philosophers"
           :size [400 400]
           :setup setup!
           :draw draw!
           :features [:keep-on-top]
           :on-close stop!)
