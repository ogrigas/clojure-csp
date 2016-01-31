(ns csp.oop
  (:require [clojure.core.match :refer [match]]
            [clojure.core.async :refer [<! >!! chan go-loop]]))

(defn new-instance [actor-fn initial-state]
  (let [state (volatile! initial-state)
        mailbox (chan 100)]
    (go-loop [] (vswap! state actor-fn (<! mailbox)) (recur))
    (fn [& args] (>!! mailbox (vec args)))))

(defn account [state message]
  (match message
    ['deposit amount]
    (update state :balance + amount)
    
    ['withdraw amount]
    (if (<= amount (:balance state))
      (update state :balance - amount)
      state)
    
    ['print-report]
    (println "Account" (:number state)
             "balance:" (:balance state))))


(def acc (new-instance account {:number "LT1234" :balance 0}))
(acc 'deposit 100)
(acc 'withdraw 20)
(acc 'withdraw 5)
(acc 'withdraw 99999)
(acc 'print-report)
