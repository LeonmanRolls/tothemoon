(ns core.visual
    (:require
      [core.utils :as u]
      [incanter.charts :as ich]
      [incanter.core :as ic]))

(defn plot-standard-candles [candles]
      (let [plot (ich/candle-stick-plot
                   :data (ic/to-dataset candles)
                   :date :unixtimestamp)]
           (ic/view plot)))

