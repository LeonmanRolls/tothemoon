(set-env!
  :dependencies
  '[[org.clojure/clojure "1.9.0-alpha10"]
    [org.clojure/core.async "0.2.395"]
    [org.clojure/test.check "0.9.0" :scope "test"]
    [org.clojure/core.logic "0.8.11"]
    [clj-http "2.2.0"]
    [org.clojure/data.json "0.2.6"]
    [clojurewerkz/envision "0.1.0-SNAPSHOT"]
    [incanter "1.5.7"]
    [http.async.client "0.5.2"]
    [org.clojure/math.combinatorics "0.1.3"]
    [cfft "0.1.0"]
    [factual/timely "0.0.3"]
    [clj-time "0.12.0"]]
  :source-paths #{"src"})

(require
  '[http.async.client :as http]
  '[clojure.spec :as s]
  '[clojure.spec.gen :as gen]
  '[clojure.spec.test :as ts :refer [check]]
  '[clojure.spec.gen :as gen]
  '[clojure.core.reducers :as rd]
  '[core.utils :as u]
  '[core.visual :as vs]
  '[core.spike :as sp]
  '[clojure.data.json :as jsn]
  '[core.datasources :as ds]
  '[core.marketcap :as cmc]
  '[core.simple :as smp]
  '[incanter.core :as ic]
  '[incanter.stats :as is]
  '[incanter.charts :as ich]
  '[incanter.datasets :as id]
  '[clj-time.core :as t]
  '[clj-time.predicates :as pr]
  '[clj-time.coerce :as c]
  '[cfft.core :as fft]
  '[clojure.math.combinatorics :as combo]
  '[clojure.test :as tst :refer [is run-tests]]
  '[timely.core :as timely]
  '[clojure.core.async :as casy :refer [<!! <! >! go chan go-loop timeout]])

(deftask my-task
         "Does nothing."
         []
         (fn [next-task]
             (fn [fileset]
                 (println "hi hi")
                 (next-task fileset))))

(comment

  (def price-hist (atom {:current-order {}
                         :open-date nil
                         :stop-loss nil
                         :buy-or-sell nil
                         :order-price nil
                         :account 1000
                         :history []}))

  @price-hist

  (add-watch price-hist :clear-old
             (fn [key atom old-state new-state]
                 (prn "-- Atom Changed --")
                 (prn "key" key)
                 (prn "atom" atom)
                 (prn "old-state" old-state)
                 (prn "new-state" new-state)))

  (load-file "src/core/utils.clj")
  (load-file "src/core/simple.clj")
  (load-file "src/core/datasources.clj")

  (swap! price-hist smp/simple-strat-perc-live (first (ds/oanda-historical "EUR_USD" "1" "M5")))

  (let [c (chan)]
       (ds/oanda-patch-order-cas c "300" "1.0625")
       (<!! c))


  (defn testing [trade-green?]
        (go
               (let [shorten (fn [stng length] (if (>= length (count stng)) stng (subs stng 0 length)))
                     c1 (chan)
                     c2 (chan)
                     order-chan (chan)
                     _ (go (println (<! order-chan)))
                     _ (ds/acc-info c1)
                     _ (ds/oanda-history-cas c2 "EUR_USD" "1" "M5")
                     {{:keys [orders balance trades positions]} :account :as account} (<! c1)

                     {trade-price :price} (when (not (empty? trades)) (first trades))
                     {stop-price :price} (when (not (empty? trades)) (first orders))
                     stop-price-num (when (not (empty? trades)) (read-string stop-price))
                     trade-price-num (when (not (empty? trades)) (read-string trade-price))
                     {:keys [tradeID]} (when (not (empty? orders)) (first orders))

                     {:keys [open high low close] :as current-candle} (first (<! c2))
                     balance-lp (shorten (str balance) 4)
                     balance-lp-two (shorten (str (* 2 (read-string balance))) 4)
                     balance-lp "10000"
                     balance-lp-two "20000"
                     low-lp (shorten (str (- low 0.0001) ) 7)
                     high-lp (shorten (str (+ high 0.0001)) 7)]

                    (when (empty? trades) (reset! trade-green? (if (< 0 (- close open)) true false)))

                    (cond
                      (empty? trades) (cond
                                        (u/green? current-candle)
                                        (do
                                          (println "empty trade green current " low-lp)
                                          (ds/oanda-open-order-cas order-chan "EUR_USD" balance-lp low-lp))

                                        :red (do
                                               (println "empty trade red current " high-lp)
                                               (ds/oanda-open-order-cas order-chan "EUR_USD" (str "-" balance-lp) high-lp)))

                      (u/green? current-candle) (cond
                                                  @trade-green? (do
                                                                 (println "current candle green, green trade " low-lp)
                                                                 (ds/oanda-patch-order-cas order-chan tradeID low-lp))
                                                  :trade-red (do
                                                               (println "current candle green, red trade " balance-lp-two)
                                                               (ds/oanda-open-order-cas order-chan "EUR_USD" (str balance-lp-two) low-lp)
                                                               (reset! trade-green? (if (< 0 (- close open)) true false))
                                                               ))

                      :current-candle-red (cond
                                            @trade-green? (do
                                                           (println "current candle red, green trade " balance-lp-two)
                                                           (ds/oanda-open-order-cas order-chan "EUR_USD" (str "-" balance-lp-two) high-lp)
                                                           (reset! trade-green? (if (< 0 (- close open)) true false))
                                                           )
                                            :trade-red (do
                                                         (println "current candle red, red trade " high-lp)
                                                         (ds/oanda-patch-order-cas order-chan tradeID high-lp))))))
        )


(go
    (let [c (chan)
          trade-green? (atom false)
          _ (u/on-the-minute-second c [4 9 14 19 24 29 34 39 44 49 54 59] [54])]
         (while true
                (println "hey-----------------------------------------" (<! c))
                (testing trade-green?)
                )))


  (go
    (let [c (chan)
          trade-green? (atom false)
          init (u/on-the-x-second c 54)]
         (while true
                (println "hey----------------" (<! c))
                (testing trade-green?)
                )))

  ; 3 6 9 12 15 18 21 24 27 30 33 36 39 42 45 48 51 54 57 0
(go
    (let [c (chan)
          trade-green? (atom false)
          _ (u/on-the-minute-second c [2 5 8 11 14 17 20 23 26 29 32 35 38 41 44 47 50 53 56 59] [54])]
         (while true
                (println "hey-----------------------------------------" (<! c))
                (testing trade-green?)
                )))


  (t/hour
    (c/from-long (:unixtimestamp (first oanda-min)))
    )

  (let [c (chan)]
       (ds/oanda-history-cas c "EUR_USD" "1" "M5")
       (<!! c))


  (def oanda-min (ds/oanda-historical "EU50_EUR" "5000" "M5"))
  (def oanda-min (ds/oanda-historical "EUR_USD" "5000" "M5"))

  ;--starthere
  (def mornins (let [dadata oanda-min]
                    (->>
                      dadata
                      vec
                      (reduce
                        (fn [x y]
                            (cond
                              (or (= 7 (-> (:unixtimestamp y) c/from-long t/hour))
                                  (= 8 (-> (:unixtimestamp y) c/from-long t/hour))
                                  ) (update-in x [(- (count x) 1)] #(conj % y))
                              :else (conj x [])))
                        [[]])
                      (filter #(not (empty? %))))))

  (map
  #(->>
    %
    first
    :uniximtestamp
    c/from-long
    t/day-of-week
    )
    mornins)

  (map
    #(println
      (->>
        %
        first
        :unixtimestamp
        c/from-long
        t/day-of-week
        ))
    mornins)

  (map
    #(:account (smp/simple-strat-perc-candleratio % 0))
    mornins)

  (->>
    (map
      #(:account (smp/simple-strat-perc-candleratio % 0))
      mornins)
    (map #(- % 1000))
    (reduce +)
    )


  (:account (smp/simple-strat-perc-candleratio (nth mornins 7) 0))



  ;--------------------------------------------------

  (def client (http/create-client :keep-alive true :request-timeout -1))
  (def c (chan))
  (http/close client)

  (def oanda-min (ds/oanda-historical "EUR_USD" "1" "M5"))
  (type oanda-min)
  oanda-min
  (vs/plot-standard-candles (ds/oanda-historical "EUR_USD" "1" "M5") )

  (swap! price-hist update-in [:history] #(apply conj % (ds/oanda-historical "EUR_USD" "1" "M5")))

  (go
     (ds/oanda-open-order-cas c "EUR_USD" "100")
     (let [rslt (<! c)]
          (println "rslt" rslt)
          (println "rslt type" (type rslt))
          (swap! price-hist update-in [:history] #(conj % rslt))
          )
     )

  (swap! price-hist update-in [:history] #(conj % "hi"))

  (go (ds/oanda-open-order-cas c "EUR_USD" "-100") (<! c))

  (let []

       (ds/oanda-price-stream-chan client "EUR_USD" c)

       (go
         (while true
                (let [{:keys [asks bids] :as raw} (jsn/read-str (<! c) :key-fn keyword)
                      currask (:price (first asks))
                      currbid (:price (first bids))]

                     (println raw)
                     (println "currask: " currask)
                     (println "currbid: " currbid)))))


  ;---- Best prediciton length
  (load-file "src/core/simple.clj")

  (def oanda-min (ds/oanda-historical "GBP_JPY" "5000" "S30"))
  (vs/plot-standard-candles oanda-min)
  (vs/plot-standard-candles (ds/oanda-historical "EUR_USD" "50" "M5"))

  (:account (smp/simple-strat-perc-candleratio (take-last 5000 oanda-min) 0))

  (go
    (let [c (chan)
          init (u/on-the-x-second c 58)]
         (while true
               (println "hey" (<! c))
                )))



  ;------------------------------------------

  (def oanda-data (ds/oanda-historical "EUR_USD" "5000" "H1"))

  (def weeks (u/h1-to-weeks oanda-data))

  (defn weeks-ratio [data]
        (->>
          (map
            (fn [x]
                {:ratio x
                 :profit (:account (smp/simple-strat-perc-candleratio data x))})
            (range 0 300 0.01))
          (sort-by :profit)
          last
          ))

  (defn weeks-ratio-filter [data]
        (->>
          (map
            (fn [x]
                {:ratio x
                 :profit (:account (smp/simple-strat-perc-candleratio data x))})
            (range 0 300 0.1))
          (filter #(> (:profit %) 1010))
          (map :ratio)
          (into #{})))

  (def mapped-weeks (map weeks-ratio-filter weeks))
  (def no-empty (filter not-empty mapped-weeks))
  (count mapped-weeks)
  (count no-empty)

  (apply clojure.set/intersection
   no-empty)

  (apply u/average
    (map (fn [x] (apply u/average x)) no-empty))

  (filter not-empty mapped-weeks)

  (apply u/average (weeks-ratio-filter (first weeks)))

  (count (map weeks-ratio-filter (take 2 weeks)))

  (map :account
       (map #(smp/simple-strat-perc-candleratio % 10) weeks))

  (def a-week (nth weeks 9))
  (weeks-ratio (take 60 a-week))
  (smp/simple-strat-perc-candleratio (take-last 60 a-week) 0.01)

  (smp/simple-strat-perc-candleratio (subvec a-week 73 96) 0.9)




  (do
    (load-file "src/core/visual.clj")
    (load-file "src/core/utils.clj")
    (load-file "src/core/datasources.clj")
    (load-file "src/core/simple.clj")
    (load-file "src/core/spike.clj")
    (load-file "src/core/marketcap.clj")
    (load-file "build.boot")
    (ts/unstrument)
    (ts/instrument)
    (run-tests 'core.utils 'core.simple))

  (ts/summarize-results
    (ts/check
      'core.utils/json-get
      {:clojure.spec.test.check/opts {:num-tests 1}}))

  (ts/summarize-results
    (ts/check
      (ts/checkable-syms)
      {:clojure.spec.test.check/opts {:num-tests 1}})))


