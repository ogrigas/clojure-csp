(ns csp.dining-philosophers.processes
  (:require [clojure.core.async :refer [<! >! >!! alts! chan go go-loop timeout]]))

(def running (atom false))

(def population 5)

(defn thread-safe [f]
  (let [ch (chan 1000)]
    (go-loop [] (apply f (<! ch)) (recur))
    (fn [& args] (>!! ch args))))

(defn start-room [log]
  (let [entry-ch (chan)
        exit-ch (chan)]
    (go-loop [occupancy 0]
      (when @running
        (log :room-occupancy occupancy)
        (let [[i ch] (alts! [entry-ch exit-ch])]
          (log :philosopher i ({entry-ch :entered, exit-ch :exited} ch))
          (recur (({entry-ch inc, exit-ch dec} ch) occupancy)))))
    [entry-ch exit-ch]))

(defn start-fork [i log]
  (let [pick-up-ch (chan)
        put-down-ch (chan)]
    (go-loop []
      (when @running
        (log :fork i :picked-up-by (<! pick-up-ch))
        (log :fork i :put-down-by (<! put-down-ch))
        (recur)))
    [pick-up-ch put-down-ch]))

(defn random-timeout [from to]
  (timeout (* 200 (+ from (rand-int (- (inc to) from))))))

(defn start-philosopher [i
                         [lfork-pick-up-ch lfork-put-down-ch]
                         [rfork-pick-up-ch rfork-put-down-ch]
                         [room-entry-ch room-exit-ch]
                         log]
  (go-loop []
    (when @running
      ;; thinking...
      (<! (random-timeout 2 7))

      (>! room-entry-ch i)
      (>! lfork-pick-up-ch i)
      (>! rfork-pick-up-ch i)

      (log :philosopher i :started-eating)
      (<! (random-timeout 5 10))
      (log :philosopher i :stopped-eating)

      (>! lfork-put-down-ch i)
      (>! rfork-put-down-ch i)
      (>! room-exit-ch i)

      (recur))))

(defn start! [log]
  (reset! running true)
  (let [room (start-room log)
        forks (mapv #(start-fork % log) (range population))]
    (doseq [i (range population) :let [i+1 (mod (inc i) population)]]
      (start-philosopher i (forks i) (forks i+1) room log))))

(defn stop! []
  (reset! running false))

(defn restart! [log]
  (stop!)
  (Thread/sleep 2200)
  (start! log))
