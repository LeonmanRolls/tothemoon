(ns core.marketcap
    (:require
      [clojure.spec :as s]
      [clj-time.coerce :as c]
      [core.utils :as u]))

(def marketcap 10000000)

(def year-second 31536000000)

(defn year-calc [{:keys [id data]}]
      (let [hitcap-time  (as-> data x
                               (:market_cap_by_available_supply x)
                               (filter #(> (last %) marketcap) x)
                               (sort-by first x)
                               (ffirst x))
            initial-price (->
                            (filter #(= (first %) hitcap-time) (:price_usd data))
                            first
                            last)


            final-price (first
                          (filter
                            #(< (+ year-second hitcap-time) (first %))
                            (:price_usd data)))]

           (if final-price
             {:currency id
              :price-diff (- (last final-price) initial-price)
              :hitcaptime (.toString (c/from-long hitcap-time))
              :final-time (.toString (c/from-long (first final-price)))}
             (str "Not enough data for: " id))))

(s/fdef coinmarketcap-data-by-id
        :args (s/cat :curr-id ::u/coinmarketcap-sym))

(defn coinmarketcap-data-by-id [curr-id]
      {:id curr-id
       :data (u/json-get
               (str
                 "https://api.coinmarketcap.com/v1/datapoints/" curr-id "/1351941414000/"
                 (System/currentTimeMillis)
                 "/"))})

(defn less-than-year-filter [data]
      (as-> data x
            (:data x)
            (:market_cap_by_available_supply x)
            (map first x)
            (> (- (last x) (first x)) year-second)))

(defn market-cap-filter [data]
      (as-> data x
            (:data x)
            (:market_cap_by_available_supply x)
            (first x)
            (last x)
            (> marketcap x)))

(defn had-market-cap-filter [data]
      (as-> data x
            (:data x)
            (:market_cap_by_available_supply x)
            (filter #(> (last %) marketcap) x)
            (not (empty? x))))



