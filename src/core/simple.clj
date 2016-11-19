(ns core.simple
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
      [clojure.core.async :as casy :refer [<! >! go chan]]
      [clojure.test :as tst :refer [is run-tests with-test]]
      [core.datasources :as ds]
      [core.utils :as u]))

(defn green? [{:keys [open close]}]
      (> close open))

(defn red? [{:keys [open close]}]
      (> open close))

(defn profit-calc-red [nexthigh high close nextclose]
      (if
        (> nexthigh high)
        (- close high)
        (- close nextclose)))

(defn profit-calc [open nextlow low nextclose close]
      (if
        (< nextlow low)
        (- low close)
        (- nextclose close)))

(s/fdef simple-strat
        :args (s/cat :data (s/coll-of ::u/standard-candle) :opts (s/? keyword?)))

(defn simple-strat
      "Basic strat backtesting price differences after in both directions"
      [data & opts]
      (let [humantime (if opts true false)]
           (reduce
             (fn [x
                  {nexttime :unixtimestamp nextopen :open nexthigh :high nextlow :low nextclose :close :as y}]
                 (cond
                   (contains? x :greens)
                   (let [{:keys [unixtimestamp open high low close] :as last} (:last x)]
                        (if (green? last)
                          {:greens (conj (:greens x) {:basetimestamp (if
                                                                       humantime
                                                                       (u/to-human unixtimestamp)
                                                                       (long unixtimestamp))
                                                      :nexttimestamp (if
                                                                       humantime
                                                                       (u/to-human nexttime)
                                                                       (long nexttime))

                                                      :profit (profit-calc open nextlow low nextclose close)
                                                      :prof-calc [(:last x) y]})
                           :reds (:reds x)
                           :last y}
                          {:greens (:greens x)
                           :reds (conj (:reds x) {:basetimestamp (if
                                                                   humantime
                                                                   (u/to-human unixtimestamp)
                                                                   (long unixtimestamp))
                                                  :nexttimestamp (if
                                                                   humantime
                                                                   (u/to-human nexttime)
                                                                   (long nexttime))

                                                  :profit (profit-calc-red nexthigh high close nextclose)
                                                  :prof-calc [(:last x) y]})
                           :last y}))
                   :else
                   (let [{:keys [unixtimestamp open high low close]} x]
                        (if (green? x)
                          {:greens [{:basetimestamp (if
                                                      humantime
                                                      (u/to-human unixtimestamp)
                                                      (long unixtimestamp))
                                     :nexttimestamp (if
                                                      humantime
                                                      (u/to-human nexttime)
                                                      (long nexttime))
                                     :profit (profit-calc open nextlow low nextclose close)
                                     :prof-calc [x y]}]
                           :reds []
                           :last y}
                          {:greens []
                           :reds [{:basetimestamp (if
                                                    humantime
                                                    (u/to-human unixtimestamp)
                                                    (long unixtimestamp))
                                   :nexttimestamp (if
                                                    humantime
                                                    (u/to-human nexttime)
                                                    (long nexttime))
                                   :profit (profit-calc-red nexthigh high close nextclose)
                                   :prof-calc [x y]}]
                           :last y}))))
             data)))

(defn simple-strat-profit-calc [simple-strat-data]
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

(defn profit-chains [profits chain-length]
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

(with-test

  (defn simple-strat-perc [standard-candles]
        (let [clear-fn (fn [deets-map]
                           (->
                             (update-in deets-map [:open-date] (fn [_] nil))
                             (update-in [:stop-loss] (fn [_] nil))
                             (update-in [:buy-or-sell] (fn [_] nil))
                             (update-in [:order-price] (fn [_] nil))))

              update-hist (fn [damap start end perc]
                              (update-in damap [:history] #(conj % {:start (u/to-human start)
                                                                    :end (u/to-human end)
                                                                    :perc perc})))

              buy-or-sell-fn (fn [acc candle high low close unixtimestamp]
                                 (cond
                                   (u/green? candle) (->
                                                       (update-in acc [:buy-or-sell] (fn [_] "buy"))
                                                       (update-in [:order-price] (fn [_] close))
                                                       (update-in [:stop-loss] (fn [_] low))
                                                       (update-in [:open-date] (fn [_] unixtimestamp)))

                                   (not (u/green? candle)) (->
                                                             (update-in acc [:buy-or-sell] (fn [_] "sell"))
                                                             (update-in [:order-price] (fn [_] close))
                                                             (update-in [:stop-loss] (fn [_] high))
                                                             (update-in [:open-date] (fn [_] unixtimestamp)))))]

             (reduce
               (fn [{:keys [stop-loss buy-or-sell order-price account history open-date] :as x}
                    {:keys [unixtimestamp open high low close] :as y}]

                   (cond
                     (= buy-or-sell "buy") (do
                                             (if
                                               (u/green? y)
                                               (cond
                                                 (< low stop-loss) (->
                                                                     (update-hist
                                                                       x
                                                                       open-date
                                                                       unixtimestamp
                                                                       (u/percentage-change order-price stop-loss :spicy))
                                                                     (update-in [:account]
                                                                                (fn [acc]
                                                                                    (* acc
                                                                                       (u/percentage-change order-price stop-loss :spicy))))
                                                                     clear-fn
                                                                     (buy-or-sell-fn y high low close unixtimestamp))
                                                 :else (->
                                                         (update-in x [:stop-loss] (fn [_] low))))

                                               (cond
                                                 (< low stop-loss) (->
                                                                     (update-hist
                                                                       x
                                                                       open-date
                                                                       unixtimestamp
                                                                       (u/percentage-change order-price stop-loss :spicy))
                                                                     (update-in [:account]
                                                                                (fn [acc]
                                                                                    (* acc
                                                                                       (u/percentage-change order-price stop-loss :spicy))))
                                                                     clear-fn
                                                                     (buy-or-sell-fn y high low close unixtimestamp))
                                                 :else (->
                                                         (update-hist
                                                           x
                                                           open-date
                                                           unixtimestamp
                                                           (u/percentage-change order-price close :spicy))
                                                         (update-in [:account]
                                                                    (fn [acc]
                                                                        (* acc
                                                                           (u/percentage-change order-price close :spicy))))
                                                         clear-fn
                                                         (buy-or-sell-fn y high low close unixtimestamp)))))


                     (= buy-or-sell "sell") (if
                                              (not (u/green? y))
                                              (cond
                                                (> high stop-loss) (->
                                                                     (update-hist
                                                                       x
                                                                       open-date
                                                                       unixtimestamp
                                                                       (u/percentage-change stop-loss order-price :spicy))
                                                                     (update-in [:account]
                                                                                (fn [acc]
                                                                                    (* acc
                                                                                       (u/percentage-change stop-loss order-price :spicy))))
                                                                     clear-fn
                                                                     (buy-or-sell-fn y high low close unixtimestamp))
                                                :else (->
                                                        (update-in x [:stop-loss] (fn [_] high))))

                                              (cond
                                                (> high stop-loss) (->
                                                                     (update-hist
                                                                       x
                                                                       open-date
                                                                       unixtimestamp
                                                                       (u/percentage-change stop-loss order-price :spicy))
                                                                     (update-in [:account]
                                                                                (fn [acc]
                                                                                    (* acc
                                                                                       (u/percentage-change stop-loss order-price :spicy))))
                                                                     clear-fn
                                                                     (buy-or-sell-fn y high low close unixtimestamp))
                                                :else (->
                                                        (update-hist
                                                          x
                                                          open-date
                                                          unixtimestamp
                                                          (u/percentage-change close order-price :spicy))
                                                        (update-in [:account]
                                                                   (fn [acc]
                                                                       (* acc
                                                                          (u/percentage-change close order-price :spicy))))
                                                        clear-fn
                                                        (buy-or-sell-fn y high low close unixtimestamp))))

                     (= buy-or-sell nil)  (do
                                            (println "hi there")
                                            (buy-or-sell-fn x y high low close unixtimestamp))))

               {:open-date nil
                :stop-loss nil
                :buy-or-sell nil
                :order-price nil
                :account 1000
                :history []}
               standard-candles)))

  (is
    (=
      (:account (simple-strat-perc u/green-reversal))
      (->
        (* 1000 (u/percentage-change 1.1 1 :hi))
        (* (u/percentage-change 0.9 1 :hi))))
    "Green reversal")

  (is
    (=
      (:account (simple-strat-perc u/red-reversal))
      (->
        (* 1000 (u/percentage-change 1.1 0.9 :hi))
        (* (u/percentage-change 1.1 1.2 :hi))))
    "red reversal")

  (is
    (=
      (* 1000 (u/percentage-change 1.2 1.1 :hi))
      (:account (simple-strat-perc u/green-stop-out))))

  (is
    (=
      (* 1000 (u/percentage-change 1.2 1.25 :hi))
      (:account (simple-strat-perc u/green->red))))

  (is
    (=
      (* 1000 (u/percentage-change 0.9 0.8  :hi))
      (:account (simple-strat-perc u/red-stop-out))))

  (is
    (=
      (* 1000 (u/percentage-change 0.72 0.8  :hi))
      (:account (simple-strat-perc u/red->green)))))


(defn simple-strat-no-stop-update [standard-candles]
      (let [clear-fn (fn [deets-map]
                         (->
                           (update-in deets-map [:open-date] (fn [_] nil))
                           (update-in [:stop-loss] (fn [_] nil))
                           (update-in [:buy-or-sell] (fn [_] nil))
                           (update-in [:order-price] (fn [_] nil))))]

           (reduce
             (fn [{:keys [stop-loss buy-or-sell order-price account history] :as x}
                  {:keys [unixtimestamp open high low close] :as y}]

                 (cond
                   (= buy-or-sell "buy") (do
                                           (println "buy")
                                           (if
                                             (u/green? y)
                                             (cond
                                               (< low stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change order-price stop-loss :spicy))))
                                                                   clear-fn)
                                               :else x)

                                             (cond
                                               (< low stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change order-price stop-loss :spicy))))
                                                                   clear-fn)
                                               :else (->
                                                       (update-in x [:account]
                                                                  (fn [acc]
                                                                      (* acc
                                                                         (u/percentage-change order-price close :spicy))))
                                                       clear-fn))))


                   (= buy-or-sell "sell") (if
                                            (not (u/green? y))
                                            (cond
                                              (> high stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change stop-loss order-price :spicy))))
                                                                   clear-fn)
                                              :else x)

                                            (cond
                                              (> high stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change stop-loss order-price :spicy))))
                                                                   clear-fn)
                                              :else (->
                                                      (update-in x [:account]
                                                                 (fn [acc]
                                                                     (* acc
                                                                        (u/percentage-change order-price close :spicy))))
                                                      clear-fn)))

                   (= buy-or-sell nil)  (do
                                          (println "not buy or sell")
                                          (cond
                                            (u/green? y) (->
                                                           (update-in x [:buy-or-sell] (fn [_] "buy"))
                                                           (update-in [:order-price] (fn [_] close))
                                                           (update-in [:stop-loss] (fn [_] low))
                                                           (update-in [:open-date] (fn [_] unixtimestamp)))

                                            (not (u/green? y)) (->
                                                                 (update-in x [:buy-or-sell] (fn [_] "sell"))
                                                                 (update-in [:order-price] (fn [_] close))
                                                                 (update-in [:stop-loss] (fn [_] high))
                                                                 (update-in [:open-date] (fn [_] unixtimestamp)))))))

             {:open-date nil
              :stop-loss nil
              :buy-or-sell nil
              :order-price nil
              :account 1000
              :history []}
             standard-candles)))

(defn simple-strat-perc-green [standard-candles]
      (let [clear-fn (fn [deets-map]
                         (->
                           (update-in deets-map [:open-date] (fn [_] nil))
                           (update-in [:stop-loss] (fn [_] nil))
                           (update-in [:buy-or-sell] (fn [_] nil))
                           (update-in [:order-price] (fn [_] nil))))]

           (reduce
             (fn [{:keys [stop-loss buy-or-sell order-price account history] :as x}
                  {:keys [unixtimestamp open high low close] :as y}]

                 (cond
                   (= buy-or-sell "buy") (do
                                           (println "buy")
                                           (if
                                             (u/green? y)
                                             (cond
                                               (< low stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change order-price stop-loss :spicy))))
                                                                   clear-fn)
                                               :else (->
                                                       (update-in x [:stop-loss] (fn [_] low))))

                                             (cond
                                               (< low stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change order-price stop-loss :spicy))))
                                                                   clear-fn)
                                               :else (->
                                                       (update-in x [:account]
                                                                  (fn [acc]
                                                                      (* acc
                                                                         (u/percentage-change order-price close :spicy))))
                                                       clear-fn))))


                   (= buy-or-sell "sell") (if
                                            (not (u/green? y))
                                            (cond
                                              (> high stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change stop-loss order-price :spicy))))
                                                                   clear-fn)
                                              :else (->
                                                      (update-in x [:stop-loss] (fn [_] high))))

                                            (cond
                                              (> high stop-loss) (->
                                                                   (update-in x [:account]
                                                                              (fn [acc]
                                                                                  (* acc
                                                                                     (u/percentage-change stop-loss order-price :spicy))))
                                                                   clear-fn)
                                              :else (->
                                                      (update-in x [:account]
                                                                 (fn [acc]
                                                                     (* acc
                                                                        (u/percentage-change order-price close :spicy))))
                                                      clear-fn)))

                   (= buy-or-sell nil)  (do
                                          (println "not buy or sell")
                                          (cond
                                            (u/green? y) (->
                                                           (update-in x [:buy-or-sell] (fn [_] "buy"))
                                                           (update-in [:order-price] (fn [_] close))
                                                           (update-in [:stop-loss] (fn [_] low))
                                                           (update-in [:open-date] (fn [_] unixtimestamp)))

                                            (not (u/green? y)) x))))

             {:open-date nil
              :stop-loss nil
              :buy-or-sell nil
              :order-price nil
              :account 1000
              :history []}
             standard-candles)))


(defn simple-strat-live [candle-chan]
      (go
        (let [clear-fn (fn [deets-map]
                           (->
                             (update-in deets-map [:open-date] (fn [_] nil))
                             (update-in [:stop-loss] (fn [_] nil))
                             (update-in [:buy-or-sell] (fn [_] nil))
                             (update-in [:order-price] (fn [_] nil))))

              order-chan (chan)
              order-fn (fn [amount]
                           (ds/oanda-open-order-cas order-chan "EUR_USD" amount)
                           (<! order-chan))]

             (loop [{:keys [stop-loss buy-or-sell order-price] :as x} {:stop-loss nil
                                                                       :buy-or-sell nil
                                                                       :order-price nil}]
                   (let [{:keys [unixtimestamp open high low close] :as y} (<! chan)]
                        (cond
                          (= buy-or-sell "buy") (do
                                                  (println "buy")
                                                  (if
                                                    (u/green? y)
                                                    (cond
                                                      (< low stop-loss) (->
                                                                          (update-in x [:account]
                                                                                     (fn [acc]
                                                                                         (* acc
                                                                                            (u/percentage-change order-price stop-loss :spicy))))
                                                                          clear-fn)
                                                      :else (->
                                                              (update-in x [:stop-loss] (fn [_] low))))

                                                    (cond
                                                      (< low stop-loss) (->
                                                                          (update-in x [:account]
                                                                                     (fn [acc]
                                                                                         (* acc
                                                                                            (u/percentage-change order-price stop-loss :spicy))))
                                                                          clear-fn)
                                                      :else (->
                                                              (update-in x [:account]
                                                                         (fn [acc]
                                                                             (* acc
                                                                                (u/percentage-change order-price close :spicy))))
                                                              clear-fn))))


                          (= buy-or-sell "sell") (if
                                                   (not (u/green? y))
                                                   (cond
                                                     (> high stop-loss) (->
                                                                          (update-in x [:account]
                                                                                     (fn [acc]
                                                                                         (* acc
                                                                                            (u/percentage-change stop-loss order-price :spicy))))
                                                                          clear-fn)
                                                     :else (->
                                                             (update-in x [:stop-loss] (fn [_] high))))

                                                   (cond
                                                     (> high stop-loss) (->
                                                                          (update-in x [:account]
                                                                                     (fn [acc]
                                                                                         (* acc
                                                                                            (u/percentage-change stop-loss order-price :spicy))))
                                                                          clear-fn)
                                                     :else (->
                                                             (update-in x [:account]
                                                                        (fn [acc]
                                                                            (* acc
                                                                               (u/percentage-change order-price close :spicy))))
                                                             clear-fn)))

                          (= buy-or-sell nil)  (do
                                                 (println "not buy or sell")
                                                 (cond
                                                   (u/green? y) (->
                                                                  (update-in x [:buy-or-sell] (fn [_] "buy"))
                                                                  (update-in [:order-price] (fn [_] close))
                                                                  (update-in [:stop-loss] (fn [_] low))
                                                                  (update-in [:open-date] (fn [_] unixtimestamp)))

                                                   (not (u/green? y)) (->
                                                                        (update-in x [:buy-or-sell] (fn [_] "sell"))
                                                                        (update-in [:order-price] (fn [_] close))
                                                                        (update-in [:stop-loss] (fn [_] high))
                                                                        (update-in [:open-date] (fn [_] unixtimestamp)))))))))))

(defn simple-strat-profits
      "standard-candle bool"
      [data profits?]
      (->>
        (if profits?
          (filter #(< 1 (:perc %)) (:history (simple-strat-perc data)))
          (filter #(> 1 (:perc %)) (:history (simple-strat-perc data))))
        (map #(update-in % [:start] u/timestamp->unix))
        (map #(update-in % [:end] u/timestamp->unix))))

(defn create-chains
      "link up individual profit and loss occurances to make chains"
      [profits]
      (->>
        (loop [dadata profits]
              (let [initial-count (count dadata)
                    combo-pass (->>
                                 (partition 2 dadata)
                                 (map
                                   (fn [[a b :as part]]
                                       (if
                                         (= (:end a) (:start b))
                                         {:start (:start a) :end (:end b)}
                                         part)))
                                 flatten)
                    final-count (count combo-pass)]
                   (cond
                     (= initial-count final-count) combo-pass
                     (not= initial-count final-count) (recur combo-pass))))

        (sort-by (fn [{:keys [start end]}] (- end start)))))


(comment

  (as-> (nth big-daddy-data 11) x
        (:data x)
        (:market_cap_by_available_supply x)

        (filter #(> marketcap (last %)) x)
        )

  (year-calc (nth big-daddy-data 12))
  (had-market-cap-filter (nth big-daddy-data 120))

  (had-market-cap-filter (first big-daddy-data))
  (had-market-cap-filter "hi")

  (as-> (nth big-daddy-data 20) x
        (:data x)
        (:market_cap_by_available_supply x)
        (first x)
        (last x)
        (> marketcap x))

  (map year-calc big-daddy-data)

  (def testing (data-by-id "litecoin"))

  (def calc (year-calc testing))

  (year-calc testing)

  (count big-daddy-data)

  (def filtered (as-> big-daddy-data  x
                      (filter less-than-year-filter x)
                      (filter market-cap-filter x)
                      (filter had-market-cap-filter x)))

  (pprint (map year-calc filtered))

  (map :id filtered)

  (map year-calc filtered)

  (def example-response (cnt/get test-endpoint))

  (def test-endpoint "https://api.coinmarketcap.com/v1/datapoints/ripple/1351941414000/1478141343000/")

  (def testing (u/json-get test-endpoint))

  (ffirst (:price_btc testing))

  (apply min-key #(Math/abs (- % -4)) [-3 1 4])
  (->
    (apply min-key #(Math/abs (do
                                #_(println (- % (c/to-long (t/plus (c/from-long hitcap-time) (t/years 1)))))
                                (println %)
                                (- % (c/to-long (t/plus (c/from-long hitcap-time) (t/years 1))))
                                )) (map first (:price_usd data)))
    )

  (apply
    min-key
    #(Math/abs (- (first %) (c/to-long (t/plus (c/from-long hitcap-time) (t/years 1)))))
    )

  (ich/candle-stick-plot
    :data (reduce->mapofcol (:Data resp))
    :date :time
    )

  (ich/candle-stick-plot
    :data (ic/to-dataset (take 2 (:Data resp)))
    :date :time
    )

  )

