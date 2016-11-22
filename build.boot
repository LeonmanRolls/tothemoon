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
                     (println "currbid: " currbid)))))


  (vs/plot-standard-candles (take-last 5 stnd))
  (vs/plot-standard-candles (take-last 15 krak-eth-eur))
  (pprint (take-last 15 krak-eth-eur))

  (:account (smp/simple-strat-perc (take-last 1000 oanda-min)))

  (def oanda-raw (ds/oanda-historical-raw "EUR_USD" "5000" "H1"))

  (first (take 6 oanda-min))

  (vs/plot-between-dates (take 6 oanda-min) 1454378400000 1454389200000)

  (vs/plot-standard-candles u/red-reversal)

  (let [data (subvec (vec oanda-min) 0 4997)]
       (->>
         (map
           (fn [x]
               {:ratio x
                :profit (:account (smp/simple-strat-perc-candleratio data x))}
               )
           (range 0 300 0.01))
         (sort-by :profit)
         (take-last 30)))


  ;---- Best prediciton length
  (load-file "src/core/simple.clj")
  (simple-strat-perc-candleratio (take-last length data))
  :account

  (def oanda-min (ds/oanda-historical "EUR_USD" "5000" "M5"))
  (:account (smp/simple-strat-perc-candleratio oanda-min  0))
  (count oanda-min)

  (def eurusd (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def usdjpy (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def gbpusd (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def audusd (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def usdchf (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def usdcad (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def eurjpy (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (def eurgbp (->>
                (map
                  (partial smp/predictive-length oanda-min)
                  (range 1 5000))
                (sort-by :account)))

  (map
    :ratio
    (filter #(> (:account %) 1000) eurusd)
    )

  (:account (smp/simple-strat-perc-candleratio (subvec (vec oanda-min) 0 680) 0))





  (smp/simple-strat-perc-candleratio oanda-min 4)
  (:account (smp/simple-strat-perc-candleratio oanda-min 4))
  (smp/predictive-length oanda-min 50)

  ;4760
  (def ds (let [data (subvec (vec oanda-min) 3712 3856)]
               (->>
                 (map
                   (fn [x]
                       {:ratio x
                        :profit (:account (smp/simple-strat-perc-candleratio data x))}
                       )
                   (range 0 300 0.1))
                 ic/to-dataset)))

  (ic/with-data ds
                (ic/view
                  (ich/line-chart :ratio :profit)))







  (sort-by :profit)
  (take-last 10000)

  (ic/$order :ratio :desc ds)
  (ic/view ds)

  (ic/sel ds :cols :ratio)
  (ic/sel (ic/$order :ratio :asc ds) :cols :ratio)

  (ic/with-data (ic/$order :ratio :asc ds)
                (ic/view
                      (ich/line-chart :ratio :profit)))


                (let [dadata (ic/$order :ratio :asc ds)]
                     (ic/view
                       (ich/line-chart (ic/sel dadata :cols :ratio) (ic/sel dadata :cols :profit) )
                       )

                     )







  (let [data (take-last 48 oanda-min)]
       (->>
         (map
           (fn [x]
               {:ratio x
                :profit (:account (smp/simple-strat-perc-candleratio data x))}
               )
           (range 0 300 0.01))
          (sort-by :profit)
         (take-last 30)
         )
       )


  (:history (smp/simple-strat-perc-candleratio (take 5000 oanda-min) 150))

  (->>
    (filter #(> 1 (:perc %)) (:history (smp/simple-strat-perc-candleratio oanda-min)))
    (map #(update-in % [:start] u/timestamp->unix))
    (map #(update-in % [:end] u/timestamp->unix))
    (take 10)
    (map (fn [{:keys [start end]}] (vs/plot-between-dates oanda-min start end)))
    )

  (def losses (smp/simple-strat-profits oanda-min true))

  (def rslt (smp/create-chains losses))

  (vs/plot-between-dates oanda-min (:start (last rslt)) (:end (last rslt)))

  (def loss-length-filtered  (filter
                               (fn [{:keys [start end]}]
                                   (> (- end start) 10000000))
                               sorted))

  (def loss-length (map
                     (fn [{:keys [start end]}]
                         (- end start))
                     rslt))


  (apply
    u/average
    (map
      (fn [{:keys [start end]}]
          (- end start))
      rslt))

  (last sorted)

  (let [data (->>
               (map (fn [{:keys [start end]}] (u/to-human start)) rslt)
               (map #(clojure.string/split % #"T"))
               (map last)
               frequencies
               (sort-by first))]

       (ic/with-data (ic/to-dataset data)

                     (ic/$ :col-0)
                     (ic/$ :col-1)
                     (ic/view ic/$data)
                     (ic/view (ich/line-chart :col-0 :col-1))
                     (ic/view (ic/$order :col-1 :desc))

                     ))


  ;594
  ;start at 572
  (count sorted)

  oanda-min

  (def tue-thu (->>
                 oanda-min
                 vec
                 (reduce
                   (fn [x y]
                       (cond
                         (u/mid-week? (:unixtimestamp y)) (update-in x [(- (count x) 1)] #(conj % y))
                         :else (conj x [])))
                   [[]])
                 (filter #(not (empty? %)))))

  (smp/simple-strat-perc (nth tue-thu 38))

  (apply u/average (map :account (map smp/simple-strat-perc tue-thu)))

  ;42
  (count tue-thu)
  (nth tue-thu 38)
  (vs/plot-standard-candles (nth tue-thu 21))
  (vs/plot-standard-candles (take 72 (nth tue-thu 21)))
  (smp/simple-strat-perc (take 36 (nth tue-thu 21)))
  (pprint (:history (smp/simple-strat-perc (take 72 (nth tue-thu 38)))))

  (:history (smp/simple-strat-perc (take 48 (nth tue-thu 41))))
  (pprint (:history (smp/simple-strat-perc (take 48 (nth tue-thu 41)))))


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


