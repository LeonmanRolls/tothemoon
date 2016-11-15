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
  '[clojure.core.async :as casy :refer :all])

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

  (def oanda-min (ds/oanda-historical "EUR_USD" "5000" "S5"))

  (<!! oanda-min)

  (smp/simple-strat-perc (take-last 100 oanda-min))

  (u/to-human (:unixtimestamp (first (take-last 1 oanda-min))))



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

  (ts/summarize-results
    (ts/check
      'core.utils/json-get
      {:clojure.spec.test.check/opts {:num-tests 1}}))

  (ts/summarize-results
    (ts/check
      (ts/checkable-syms)
      {:clojure.spec.test.check/opts {:num-tests 1}})))


