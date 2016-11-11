(set-env!
  :dependencies
  '[[org.clojure/clojure "1.9.0-alpha10"]
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
  '[core.spike :as sp]
  '[core.types :as tps]
  '[core.oanda :as oa]
  '[core.datasources :as ds]
  '[core.marketcap :as cmc]
  '[core.simple :as smp])

(deftask my-task
         "Does nothing."
         []
         (fn [next-task]
             (fn [fileset]
                 (println "hi hi")
                 (next-task fileset))))

(comment

  (def client (http/create-client :keep-alive true :request-timeout -1))

  (def stream (ds/oanda-price-stream-start client println "EUR_USD"))

  (http/close client)

   (ds/oanda-historical "EUR_USD" "5000" "M1")

  (def eur-usd-historical (u/json-get
                            (str oa/rest-api-base "candles")
                            {:headers {:Authorization (str "Bearer " oa/oanda-api-key)}
                             :query-params {"instrument" "EUR_USD"
                                            "count" "5000"
                                            "granularity" "M1"}}))

  (def oanda-standard-candle (map
                               ds/oanda-candle->standard
                               (:candles eur-usd-historical)))

  (smp/simple-strat (take-last 4 oanda-standard-candle))

  (pprint
    (smp/simple-strat (take-last 8 oanda-standard-candle) :human))

  (->>
    (map
      :profit
      (:reds
        (smp/simple-strat
          (take-last 5000 oanda-standard-candle))))
    (reduce +))

  (->>
    (map
      :profit
      (:greens
        (smp/simple-strat
          (take-last 5000 oanda-standard-candle))))
    (reduce +))

  (do
    (load-file "src/core/types.clj")
    (load-file "src/core/utils.clj")
    (load-file "src/core/datasources.clj")
    (load-file "src/core/oanda.clj")
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
      {:clojure.spec.test.check/opts {:num-tests 1}}))
  )


