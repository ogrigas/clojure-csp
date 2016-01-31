(ns csp.dining-philosophers.animation
  (:require [clojure.core.match :refer [match]]
            [quil.core :refer :all]
            [csp.dining-philosophers.processes :refer :all]))

(defn update-state! [ks v]
  (swap! (state-atom) assoc-in ks v))

(defn handle-event [& args]
  (match (vec args)
    [:room-occupancy n]              (println "Room occupancy is now" n)
    [:philosopher i :entered]        (update-state! [:philosophers i] :inside)
    [:philosopher i :exited]         (update-state! [:philosophers i] :outside)
    [:fork i :picked-up-by _]        (update-state! [:forks i] :held)
    [:fork i :put-down-by _]         (update-state! [:forks i] :lying)
    [:philosopher i :started-eating] (update-state! [:plates i] :used)
    [:philosopher i :stopped-eating] (update-state! [:plates i] :not-used)))

(defn setup! []
  (frame-rate 1)
  (set-state!
    :philosophers (vec (repeat population :outside))
    :forks        (vec (repeat population :lying))
    :plates       (vec (repeat population :not-used)))
  (start! handle-event))

(defn draw! []
  (background 255)
  (fill 255 0 0)
  (with-translation [(/ (width) 2) (/ (height) 2)]
    (ellipse 0 0 200 200)
    (let [s (state)]
      (println s))))

(defsketch dpsketch
           :title "Dining Philosophers"
           :size [400 400]
           :setup setup!
           :draw draw!
           :features [:keep-on-top]
           :on-close stop!)
