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
      [core.types :as tps]
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

(defn simple-strat
      "Basic strat backtesting price differences after in both directions"
      [data & opts]
      (let [humantime (if opts true false)]
           (reduce
             (fn [x
                  {nexttime :time nextopen :open nexthigh :high nextlow :low nextclose :close :as y}]
                 (cond
                   (contains? x :greens)
                   (let [{:keys [time open high low close] :as last} (:last x)]
                        (if (green? last)
                          {:greens (conj (:greens x) {:basetimestamp (if
                                                                       humantime
                                                                       (u/to-human time)
                                                                       (long time))
                                                      :nexttimestamp (if
                                                                       humantime
                                                                       (u/to-human nexttime)
                                                                       (long time))

                                                      :profit (profit-calc open nextlow low nextclose close)
                                                      :prof-calc [(:last x) y]})
                           :reds (:reds x)
                           :last y}
                          {:greens (:greens x)
                           :reds (conj (:reds x) {:basetimestamp (if
                                                                   humantime
                                                                   (u/to-human time)
                                                                   (long time))
                                                  :nexttimestamp (if
                                                                   humantime
                                                                   (u/to-human nexttime)
                                                                   (long time))

                                                  :profit (profit-calc-red nexthigh high close nextclose)
                                                  :prof-calc [(:last x) y]})
                           :last y}))
                   :else
                   (let [{:keys [time open high low close]} x]
                        (if (green? x)
                          {:greens [{:basetimestamp (if
                                                      humantime
                                                      (u/to-human time)
                                                      (long time))
                                     :nexttimestamp (if
                                                      humantime
                                                      (u/to-human nexttime)
                                                      (long time))
                                     :profit (profit-calc open nextlow low nextclose close)
                                     :prof-calc [x y]}]
                           :reds []
                           :last y}
                          {:greens []
                           :reds [{:basetimestamp (if
                                                    humantime
                                                    (u/to-human time)
                                                    (long time))
                                   :nexttimestamp (if
                                                    humantime
                                                    (u/to-human nexttime)
                                                    (long time))
                                   :profit (profit-calc-red nexthigh high close nextclose)
                                   :prof-calc [x y]}]
                           :last y}))))
             data)))

(defn simple-strat-profit-calc [simple-strat-data]
      (->>
        simple-strat-data
        :greens
        (map :profit)
        (reduce +)))


(comment

  (simple-strat-profit-calc
    (simple-strat (:Data resp)))

  ;Strategy data for adding to chart
  (def ss (simple-strat (:Data resp)))

  (first (:greens ss))

  (def timez (map
               #(update-in % [:time] long)
               (:Data resp)))



  (let [plot (ich/candle-stick-plot
               :data (ic/to-dataset timez)
               :date :time)]

       (ic/view plot)


       )

  (ic/view
    (ich/candle-stick-plot
      :data (ic/to-dataset timez)
      :date :time))

  (map #(ich/add-pointer plot (:unixtimestamp %) (:price %)))




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

  (map first (:price_usd data))
  (map first (:price_usd data))
  first
  c/from-long
  (apply
    min-key
    #(Math/abs (- (first %) (c/to-long (t/plus (c/from-long hitcap-time) (t/years 1)))))
    )

  (first (:price_usd data))

  (c/to-long (t/plus (c/from-long hitcap-time) (t/years 1)))

  (first (:price_usd data))

  (apply min-key #(Math/abs (- % 5)) [0 0 1 6  20 11])

  (apply min-key identity [2 3 4 5])


  (as-> example-response x
        (:body x)
        (jsn/read-str x :key-fn keyword)
        (keys x))

  ;----------------- Year calc


  (let [data (:Data hourresp)]
       (->>
         (reduce
           (fn [x
                {nexttime :time nextopen :open nexthigh :high nextlow :low nextclose :close :as y}]
               (let []
                    (cond
                      (contains? x :greens)
                      (let [{:keys [time open high low close] :as last} (:last x)]
                           (if (green? last)
                             {:greens (conj (:greens x) {:basetimestamp (u/to-human time)
                                                         :nexttimestamp (u/to-human nexttime)
                                                         :profit (profit-calc open nextlow low nextclose close)
                                                         :prof-calc [(:last x) y]})
                              :last y}
                             {:greens (:greens x)
                              :last y}))
                      :else
                      (let [{:keys [time open high low close]} x]
                           (if (green? x)
                             {:greens [{:basetimestamp (u/to-human time)
                                        :nexttimestamp (u/to-human nexttime)
                                        :profit (profit-calc open nextlow low nextclose close)
                                        :prof-calc [x y]}]
                              :last y}
                             {:greens []
                              :last y})))))
           data)
         :greens
         (map :profit)
         (reduce +)
         )
       )






  ;---------------------------------------

  (:Data resp)

  (defn map->mapofcol [damap]
        (into {} (map (fn [x] [(first x) [(last x)]]) damap)))

  (defn series-insert [series damap]
        (apply
          merge
          (map
            (fn [y]
                {(first y) (conj (last y) ((first y) damap))})
            series)))

  (defn reduce->mapofcol [collofmap]
        (reduce
          (fn [x y]
              (cond
                (vector? (val (first x))) (series-insert x y)
                :else (series-insert (map->mapofcol x) y)))
          collofmap))

  (keys
    (reduce->mapofcol (:Data resp)))

  (ich/candle-stick-plot
    :data (reduce->mapofcol (:Data resp))
    :date :time
    )

  (ich/candle-stick-plot
    :data (ic/to-dataset (take 2 (:Data resp)))
    :date :time
    )

  ;(candle-stick-plot :data {:a [1 2 3]})
  (defn greet [& options]
        (apply str "Hello there, " rest))
  (greet "one" "two" "three" "four")

  (assoc {} :hi "there")

  (fn [& args] args)

  (apply
    (concat
      (mapcat #(vector % %) [:volume :high :low :open :close :date])
      (apply concat
             (seq (apply assoc {}
                         [:main-title ""
                          :time-label ""
                          :value-label ""
                          :series-label ""]))

             )
      )
    )

  (type (:time (first (:Data resp))))
  (long (:time (first (:Data resp))))

  (let [data (:Data hourresp)]
       (->>
         (reduce
           (fn [x
                {nexttime :time nextopen :open nexthigh :high nextlow :low nextclose :close :as y}]
               (let []
                    (cond
                      (contains? x :greens)
                      (let [{:keys [time open high low close] :as last} (:last x)]
                           (if (green? last)
                             {:greens (conj (:greens x) {:basetimestamp (to-human time)
                                                         :nexttimestamp (to-human nexttime)
                                                         :profit (profit-calc open nextlow low nextclose close)
                                                         :prof-calc [(:last x) y]})
                              :last y}
                             {:greens (:greens x)
                              :last y}))
                      :else
                      (let [{:keys [time open high low close]} x]
                           (if (green? x)
                             {:greens [{:basetimestamp (to-human time)
                                        :nexttimestamp (to-human nexttime)
                                        :profit (profit-calc open nextlow low nextclose close)
                                        :prof-calc [x y]}]
                              :last y}
                             {:greens []
                              :last y})))))
           data)
         :greens
         (map :profit)
         (reduce +)
         )
       )

  )

