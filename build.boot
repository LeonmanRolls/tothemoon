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
  '[clojure.test :as tst :refer [is run-tests]]
  '[clojure.core.async :as casy :refer [<! >! go chan]])

(deftask my-task
         "Does nothing."
         []
         (fn [next-task]
             (fn [fileset]
                 (println "hi hi")
                 (next-task fileset))))

(comment

  (def stream (ds/oanda-price-stream-start client println "EUR_USD"))
  (http/close client)

  (def hist (ds/oanda-historical "EUR_USD" "5000" "D"))

  (let [plot (ich/candle-stick-plot
               :data (ic/to-dataset santz)
               :date :unixtimestamp)]

       (ic/view plot))

  (drop-last (take-last 5 (:candles hist)))

  (def santz (map ds/oanda-candle->standard (:candles hist)))

  (def smpl-strat (smp/simple-strat (take-last 10 santz)))
  (def smpl-strat (smp/simple-strat santz))

  ;-------------------------------------------------------------------------

  {:unixtimestamp 234234324
   :avgprice 234.32432}

  (def price-hist (atom []))

  (add-watch price-hist :clear-old
            (fn [key atom old-state new-state])

             )


  (def client (http/create-client :keep-alive true :request-timeout -1))
  (def c (chan))
  (http/close client)

  (go
     (ds/oanda-open-order-cas c "EUR_USD" "1000")
     (println (<! c)))

  (go
     (ds/oanda-open-order-cas c "EUR_USD" "-1000")
     (println (<! c)))

  (go
    (ds/oanda-open-order-cas c "EUR_USD" "2000")
    (println (<! c)))

  (go
    (ds/oanda-open-order-cas c "EUR_USD" "-2000")
    (println (<! c)))

  (let []

       (ds/oanda-price-stream-chan client "EUR_USD" c)

       (go
         (while true
                (let [{:keys [asks bids] :as raw} (jsn/read-str (<! c) :key-fn keyword)
                      currask (:price (first asks))
                      currbid (:price (first bids))]

                     (println raw)
                     (println "currask: " currask)
                     (println "currbid: " currbid)

                     )
                ))

       )

  (load-file "src/core/datasources.clj")

  ;------------------------------------------------------------------------

  (def crypto-hist (ds/cryptocompare-hist "BTC" "USD" "histoday" 1000))

  (pprint
    (smp/simple-strat
      (take-last 10 crypto-hist)
      :human))

  (smp/simple-strat-profit-calc
      (smp/simple-strat
        (take-last 720 crypto-hist)
        :human))

 (u/update-all-vals
   (take-last 10 crypto-hist)
   [:unixtimestamp]
   u/to-human)

  (def rslt (u/json-get
              "https://api.kraken.com/0/public/OHLC"
              {:query-params {:pair "XBTUSD" :interval "1440"}}))

  (def eth-eur
    (u/json-get
      "https://api.kraken.com/0/public/OHLC"
      {:query-params {:pair "ETHEUR" :interval "1440"}}))

  (def oanda-min (ds/oanda-historical "EUR_USD" "5000" "5S"))

  (keys (:candles oanda-min))
  (map ds/oanda-candle->standard (:candles oanda-min))

  (def stnd (ds/kraken-hist->standard (:XXBTZUSD (:result rslt))))

  (def crypt-krak (ds/cryptocompare-hist "BTC" "USD" "histoday" 720 "Kraken"))

  (def krak-eur (ds/kraken-hist->standard (:XXBTZEUR (:result xbt-eur))))

  (def krak-eth-btc (ds/kraken-hist->standard (:XETHXXBT (:result eth-btc))))

  (def krak-eth-eur (ds/kraken-hist->standard (:XETHZEUR (:result eth-eur))))

  (vs/plot-standard-candles (take-last 15 stnd))
  (vs/plot-standard-candles (take-last 15 crypt-krak))
  (vs/plot-standard-candles (take-last 15 krak-eur))
  (vs/plot-standard-candles (take-last 15 krak-eth-btc))
  (vs/plot-standard-candles (take-last 15 krak-eth-eur))

  (smp/simple-strat-profit-calc
    (smp/simple-strat
      (take-last 250 stnd)
      :human))

  (smp/simple-strat-profit-calc
    (smp/simple-strat
      (take-last 720 krak-eur)
      :human))

  (smp/simple-strat-profit-calc
    (smp/simple-strat
      (take-last 250 krak-eur)
      :human))

  (vs/plot-standard-candles (take-last 5 stnd))
  (vs/plot-standard-candles (take-last 15 krak-eth-eur))
  (pprint (take-last 15 krak-eth-eur))

  (def oanda-min (ds/oanda-historical "EUR_USD" "5000" "H1"))

  (let [section (subvec (vec oanda-min) 4970 4990)]
       (vs/plot-standard-candles section)
       (smp/simple-strat-perc section))


  (smp/simple-strat-no-stop-update (take-last 5000 oanda-min))
  (smp/simple-strat-perc (take-last 27 oanda-min))
  (vs/plot-standard-candles (subvec (vec oanda-min) 4990 5000))

  (smp/simple-strat-profit-calc (smp/simple-strat (take-last 4000 oanda-min)))
  (u/to-human (:unixtimestamp (first (take-last 1 oanda-min))))

  (def oanda-raw (ds/oanda-historical-raw "EUR_USD" "5000" "H1"))

  (pr/weekday? (:time (first oanda-raw)))

  (t/date-time (:time (first oanda-raw)))

  (pr/thursday? (c/from-long (:unixtimestamp (first oanda-min))))

  (mid-week? (:unixtimestamp (nth oanda-min 160)))

  (def tue-thu (->>
                 (reduce
                   (fn [x y]
                       (cond
                         (u/mid-week? (:unixtimestamp y)) (update-in x [(- (count x) 1)] #(conj % y))
                         :else (conj x [])))
                   [[]]
                   (vec oanda-min))
                 (filter #(not (empty? %)))))

  (smp/simple-strat-perc (last tue-thu))

  (apply
    u/average
  (map
    :account
  (map smp/simple-strat-no-stop-update tue-thu)
    )
    )

  ;43
  (count tue-thu)
  (nth tue-thu 41)
  (:history (smp/simple-strat-perc (take 48 (nth tue-thu 41))))
  (pprint (:history (smp/simple-strat-perc (take 48 (nth tue-thu 41)))))
  (vs/plot-standard-candles (take 48 (nth tue-thu 41)))

  (type oanda-min)

  (def green-stop-out [{:open 1 :high 1.3 :low 0.9 :close 1.2 :unixtimestamp 1479347761}
                       {:open 1.2 :high 1.4 :low 1.1 :close 1.3 :unixtimestamp 1479358762}
                       {:open 1.3 :high 1.5 :low 1 :close 1.4 :unixtimestamp 1479369763}])

  (def green->red [{:open 1 :high 1.3 :low 0.9 :close 1.2 :unixtimestamp 1479346761}
                       {:open 1.2 :high 1.4 :low 1.1 :close 1.3 :unixtimestamp 1479356762}
                       {:open 1.3 :high 1.5 :low 1.15 :close 1.2 :unixtimestamp 1479366763}])

  (def red-stop-out [{:open 1 :high 1.1 :low 0.7 :close 0.8 :unixtimestamp 1479346761}
                     {:open 0.8 :high 0.9 :low 0.6 :close 0.7 :unixtimestamp 1479356762}
                     {:open 0.7 :high 1 :low 0.5 :close 0.6 :unixtimestamp 1479366763}])

  (def red->green [{:open 1 :high 1.1 :low 0.7 :close 0.8 :unixtimestamp 1479346761}
                     {:open 0.8 :high 0.9 :low 0.6 :close 0.7 :unixtimestamp 1479356762}
                     {:open 0.7 :high 0.75 :low 0.5 :close 0.6 :unixtimestamp 1479366763}])

  (is
    (=
      (* 1000 (u/percentage-change 1.2 1.1 :hi))
      (:account (smp/simple-strat-perc green-stop-out))))


  (smp/simple-strat-perc green-stop-out)
  (vs/plot-standard-candles green->red)
  (vs/plot-standard-candles (subvec (vec oanda-min) 4990 5000))

  (do
    (load-file "src/core/visual.clj")
    (load-file "src/core/utils.clj")
    (load-file "src/core/datasources.clj")
    (load-file "src/core/simple.clj")
    (load-file "src/core/spike.clj")
    (load-file "src/core/marketcap.clj")
    (load-file "build.boot")
    (ts/unstrument)
    (ts/instrument))

  (run-tests 'core.utils)

  (ts/summarize-results
    (ts/check
      'core.utils/json-get
      {:clojure.spec.test.check/opts {:num-tests 1}}))

  (ts/summarize-results
    (ts/check
      (ts/checkable-syms)
      {:clojure.spec.test.check/opts {:num-tests 1}})))


