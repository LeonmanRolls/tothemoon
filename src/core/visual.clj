(ns core.visual
    (:require
      [core.utils :as u]
      [core.simple :as smp]
      [incanter.charts :as ich]
      [incanter.core :as ic]))

(defn plot-standard-candles [candles]
      (let [plot (ich/candle-stick-plot
                   :data (ic/to-dataset candles)
                   :date :unixtimestamp)]
           (ic/view plot)))

(defn plot-chains [data r-or-g chain-length]
      (->>
        (smp/profit-chains (r-or-g (smp/simple-strat data)) chain-length)
        (map u/first-and-last)
        (map
          (fn [x]
              (->
                (u/candles-between
                  (-> x first :basetimestamp)
                  (-> x last :basetimestamp)
                  data)
                (plot-standard-candles))))))

(defn plot-between-dates [candles start end]
      (->
        (filter #(<= start (:unixtimestamp %) end) candles)
        plot-standard-candles))


