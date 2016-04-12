(ns life-without-objects)


(def Account
  {'deposit  (fn [this amount]
               (update this :balance + amount))

   'withdraw (fn [this amount]
               (if (< amount (this :balance))
                 (update this :balance - amount)
                 this))})


(def state0 {:balance 0})
(def state1 ((Account 'deposit)  state0 100))
(def state2 ((Account 'withdraw) state1 30))
(def state3 ((Account 'withdraw) state2 20))

state0
state1
state2
state3


(defn new-object [clazz initial-state]
  (let [state (atom initial-state)]
    (fn [method & args]
      (apply swap! state (clazz method) args))))


(def acc (new-object Account {:balance 0}))
(acc 'deposit 100)
(acc 'withdraw 30)
(acc 'withdraw 20)
