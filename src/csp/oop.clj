(ns csp.oop
  (:require [clojure.core.match :refer [match]]
            [clojure.core.async :refer [<! >!! chan go-loop]]))

(defn new-instance [actor-fn initial-state]
  (let [state (volatile! initial-state)
        mailbox (chan 100)]
    (go-loop [] (actor-fn state (<! mailbox)) (recur))
    (fn [& args] (>!! mailbox (vec args)))))

(defn account [state message]
  (match message
    ['deposit amount]
    (vswap! state update :balance + amount)

    ['withdraw amount]
    (if (<= amount (:balance @state))
      (vswap! state update :balance - amount)
      (println "Overdrafts not allowed"))

    ['print-report]
    (println "Account" (:number @state)
             "balance:" (:balance @state))))


(comment

  (def acc (new-instance account {:number "LT1234" :balance 0}))

  (acc 'deposit 100)
  (acc 'print-report)

  (acc 'withdraw 20)
  (acc 'print-report)

  (acc 'withdraw 99999)
  (acc 'print-report))
