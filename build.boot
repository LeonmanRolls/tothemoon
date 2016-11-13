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
  '[core.visual :as vs]
  '[core.spike :as sp]
  '[clojure.data.json :as jsn]
  '[core.datasources :as ds]
  '[core.marketcap :as cmc]
  '[core.simple :as smp]
  '[incanter.core :as ic]
  '[incanter.stats :as is]
  '[incanter.charts :as ich]
  '[incanter.datasets :as id])

(deftask my-task
         "Does nothing."
         []
         (fn [next-task]
             (fn [fileset]
                 (println "hi hi")
                 (next-task fileset))))

(def open-orders (atom []))

(comment

  (def client (http/create-client :keep-alive true :request-timeout -1))
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

  ;------------------------------------------------------------------------

  (def crypto-hist (ds/cryptocompare-hist "BTC" "USD" "histoday" 1000))

  (pprint
    (smp/simple-strat
      (take-last 10 crypto-hist)
      :human))

  (smp/simple-strat-profit-calc
      (smp/simple-strat
        (take-last 10 crypto-hist)
        :human))

 (u/update-all-vals
   (take-last 10 crypto-hist)
   [:unixtimestamp]
   u/to-human)

  ;Turn this into a funciton
  (as-> (smp/simple-strat (take-last 1000 crypto-hist)) x
        (:greens x)
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
        (filter #(<= 6 (count %)) x)
        )


  (->
    (u/candles-between
      "2014-08-21T00:00:00.000Z"
      "2014-09-30T00:00:00.000Z"
      crypto-hist)
    (vs/plot-standard-candles)
    )

  ;Get first and last elements from list

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


