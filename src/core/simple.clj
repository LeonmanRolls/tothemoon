(ns core.simple
  "##Simpel strat
  This is the implementation of a very simple trading strategy based on candlesticks.  
  If the last candle was green, buy, and put a stop loss at the low of the last candle, on the sell side, sell and put 
  a stop loss at the high of the last candle. Repeat each time a new candle appears (closing the previous trade). 
  Equivalently we can leave a trade open if it is being made in the same direction as the previous trade and just 
  update the stop loss."
  (:require
    [clj-http.client :as cnt]
    [clojure.data.json :as jsn]
    [clojure.spec :as s]
    [clojure.spec.gen :as gen]
    [clojure.spec.test :as ts :refer [check]]
    [clojure.spec.gen :as gen]
    [incanter.core :as ic]
    [incanter.stats :as is]
    [incanter.charts :as ich]
    [incanter.datasets :as id]
    [clj-time.core :as t]
    [clj-time.format :as f]
    [clj-time.coerce :as c]
    [clojure.set :as set]
    [clojure.core.logic :as lgc]
    [clojure.core.async :as casy :refer [<!! <! >! go chan]]
    [clojure.test :as tst :refer [is run-tests with-test]]
    [core.datasources :as ds]
    [core.utils :as u]))

(declare simple-strat-final-profit)

(defn green? 
 "Is the candlestick green?" 
  [{:keys [open close]}]
      (> close open))

(defn red? 
 "Is the candlestick red?" 
  [{:keys [open close]}]
      (> open close))

(defn profit-calc-red 
 "Helper for simple strat." 
  [nexthigh high close nextclose]
      (if
        (> nexthigh high)
        (- close high)
        (- close nextclose)))

(defn profit-calc 
 "Helper for simple strat" 
  [open nextlow low nextclose close]
      (if
        (< nextlow low)
        (- low close)
        (- nextclose close)))

(defn clear-fn 
  "Clear mock account data to simulate closing a trade. Leaves history intact" 
  [mock-acc]
  (->
    (update mock-acc :open-date (fn [_] nil))
    (update :stop-loss (fn [_] nil))
    (update :buy-or-sell (fn [_] nil))
    (update :order-price (fn [_] nil))))

(defn human-or-long 
  "Return human or long timestamps" 
  [human? unixtimestamp]
  (if
    human?
    (u/to-human unixtimestamp)
    (long unixtimestamp)))

(defn simple-strat-profit-calc 
  "Add total profits/losses from buying and selling to get final profit/loss" 
  [simple-strat-data]
  (+
    (->>
      simple-strat-data
      :greens
      (map :profit)
      (reduce +))
    (->>
      simple-strat-data
      :reds
      (map :profit)
      (reduce +))))

(s/fdef simple-strat
        :args (s/cat :data (s/coll-of ::u/standard-candle) :opts (s/? keyword?)))

(with-test

  (defn simple-strat
    "Basic strat profit calculation separating buy and sell side. Can return human or unix timestamps."
    [data & opts]
    (let [humantime (if opts true false)]
      (reduce
        (fn [x
             {nexttime :unixtimestamp nextopen :open nexthigh :high nextlow :low nextclose :close :as y}]
          (cond
            (contains? x :greens)
            (let [{:keys [unixtimestamp open high low close] :as last} (:last x)]
              (if (green? last)
                {:greens (conj (:greens x) {:basetimestamp (human-or-long humantime unixtimestamp)
                                            :nexttimestamp (human-or-long humantime nexttime)
                                            :profit (profit-calc open nextlow low nextclose close)
                                            :prof-calc [(:last x) y]})
                 :reds (:reds x)
                 :last y}
                {:greens (:greens x)
                 :reds (conj (:reds x) {:basetimestamp (human-or-long humantime unixtimestamp)
                                        :nexttimestamp (human-or-long humantime nexttime)

                                        :profit (profit-calc-red nexthigh high close nextclose)
                                        :prof-calc [(:last x) y]})
                 :last y}))
            :else
            (let [{:keys [unixtimestamp open high low close]} x]
              (if (green? x)
                {:greens [{:basetimestamp (human-or-long humantime unixtimestamp)
                           :nexttimestamp (human-or-long humantime nexttime)
                           :profit (profit-calc open nextlow low nextclose close)
                           :prof-calc [x y]}]
                 :reds []
                 :last y}
                {:greens []
                 :reds [{:basetimestamp (human-or-long humantime unixtimestamp)
                         :nexttimestamp (human-or-long humantime nexttime)
                         :profit (profit-calc-red nexthigh high close nextclose)
                         :prof-calc [x y]}]
                 :last y}))))
        data))) 

  (is 
    (= 
      0.1 
      (simple-strat-final-profit u/green->green))
    "green->green, no stop out")

  (is 
    (= 
      -0.1 
      (simple-strat-final-profit u/green->red))
    "green->red, no stop out"))

(defn simple-strat-final-profit
  "Final profit from buy and sell side for simple strat, optional rounding" 

  ([standard-candles] 
   (simple-strat-final-profit standard-candles 1))

  ([standard-candles round-to]
   (->> 
     (simple-strat standard-candles)  
     (simple-strat-profit-calc)
     (u/round2 round-to))))

(defn profit-chains 
  ""
  [profits chain-length]
      (as-> profits x
            (reduce
              (fn [x {:keys [profit] :as y}]
                  (cond
                    (> 0 profit) (update-in
                                   x
                                   [(- (count x) 1)]
                                   (fn [a] (conj a
                                                 (->
                                                   (select-keys y [:profit :basetimestamp])
                                                   (update-in [:basetimestamp] u/to-human)))))
                    (< 0 profit) (conj x [])
                    :else x))
              [[]]
              x)
            (filter #(<= chain-length (count %)) x)))

;##Simple start algorithm (backtesting)
;For backtesting so implemented as a reducer that operates on historical data. 
;For this strategy the three states we can be in are buy, sell and resting.  
;
;Resting will only happen during the first iteration when no trade has been made yet. Buy and sell states are mirror 
;images of eachother. If we are in a buy trade the next candle could be green or red. If it is green it could have 
;hit our stop loss, so we close the trade and open a new one, otherwise we just update our stop loss. If it is red 
;we will definitely be closing and starting a new trade, only question is did it hit our stop loss or not.
;
;Put simply we either get stopped out, close the current trade at the close of a red canlde or update our stop loss.

(defn update-mock-acc
  "Update mock account with order data" 
  [acc buy-or-sell close low unixtimestamp]
  (->
    (update acc :buy-or-sell (fn [_] "buy"))
    (update :order-price (fn [_] close))
    (update :stop-loss (fn [_] low))
    (update :open-date (fn [_] unixtimestamp))))

(s/fdef buy-or-sell-judge 
        :args (s/cat :acc ::u/mock-account 
                     :candle any? 
                     :high any? 
                     :low any? 
                     :close any? 
                     :unixtimestamp any? 
                     :ratio (s/? (s/nilable number?))
                     :opts (s/? any?)))

(defn buy-or-sell-judge 
  "Decide to buy or sell based on colour of last candle. Can optionally take into account ratio of last candle. Updates 
  a local representation of mock account data. If live option is true then will place these orders with oanda too."

  ([acc candle high low close unixtimestamp]
   (buy-or-sell-judge acc candle high low close unixtimestamp nil))

  ([acc candle high low close unixtimestamp ratio & opts]
   (let [{:keys [live instrument units]} opts]
     (cond
       (if ratio (> ratio (u/body-ratio candle)) false) acc

       (u/green? candle) (do 
                           (when live 
                             (ds/oanda-open-order-cas! instrument units :stoploss low))
                           (update-mock-acc acc "buy" close low unixtimestamp))

       (not (u/green? candle)) (do 
                                 (when live 
                                   (ds/oanda-open-order-cas! instrument (- units) :stoploss high))
                                 (update-mock-acc acc "sell" close high unixtimestamp))))))

(s/fdef update-hist 
        :args (s/cat :mock-account #(contains? % :history) :start any? :end any? :old-price any? :new-price any?))

(defn update-hist 
  "Update transaction history in mock account by conjing the next history entry" 
  [mock-account start end old-price new-price]
  (update mock-account :history #(conj % {:start (u/to-human start)
                                          :end (u/to-human end)
                                          :perc (u/percentage-change old-price new-price :multiple)})))

(defn update-account 
  "Update model account value"
  [mock-acc old-price new-price]
  (update mock-acc :account
          (fn [acc]
            (* acc
               (u/percentage-change old-price new-price :spicy)))))

(defn simple-strat-next-position
  "Closes current trade and opens the next one for simple strat"
  [{:keys [stop-loss buy-or-sell order-price account history open-date] :as mock-account}
   {:keys [unixtimestamp open high low close] :as current-candle} 
   old-price 
   new-price
   ratio]
  (->
    (update-hist mock-account open-date unixtimestamp old-price new-price)
    (update-account old-price new-price)
    clear-fn
    (buy-or-sell-judge current-candle high low close unixtimestamp ratio)))

(defn update-stop-loss
  "Update stop loss in mock account data for simple strat" 
  [mock-acc stop-loss]
  (update mock-acc :stop-loss (fn [_] stop-loss)))

(with-test

  (defn simple-strat-perc-candleratio 
    "Simple strat implemented as a reducer for operating on historical data. Optionally a trade can be opened based on 
    the ratio between the candle body and its wicks." 
    [standard-candles ratio]
    (reduce
      (fn [{:keys [stop-loss buy-or-sell order-price account history open-date] :as x}
           {:keys [unixtimestamp open high low close] :as y}]

        (cond
          (= buy-or-sell "buy") (cond
                                  (< low stop-loss) (simple-strat-next-position x y order-price stop-loss ratio)
                                  (u/green? y) (update-stop-loss x low)
                                  :red (simple-strat-next-position x y order-price close ratio))

          (= buy-or-sell "sell") (cond
                                   (< low stop-loss) (simple-strat-next-position x y stop-loss order-price ratio)
                                   (not (u/green? y)) (update-stop-loss x high)
                                   :green (simple-strat-next-position x y close order-price ratio))

          :resting  (buy-or-sell-judge x y high low close unixtimestamp ratio)))

      {:open-date nil :stop-loss nil :buy-or-sell nil :order-price nil :account 1000 :history []}
      standard-candles))

  (is
    (=
      (* 1000 (u/percentage-change 1.2 1.1 :hi))
      (:account (simple-strat-perc-candleratio u/green->green->green-stop-out 0.4)))))

(with-test

  (defn simple-strat-perc [standard-candles]
    (simple-strat-perc-candleratio standard-candles nil))

  (is
    (=
      (* 1000 (u/percentage-change 1.2 1.1 :hi))
      (:account (simple-strat-perc u/green->green->green-stop-out)))))

