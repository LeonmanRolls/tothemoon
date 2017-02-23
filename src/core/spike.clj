(ns core.spike
  "Spike theory or 'pump and dump'. This started with noticing that many crypto currencies at some point in their 
  existence go through a sharp rise followed by sharp fall in price. The goal of this strategy is to try to identify 
  when a spike is happening early enough to get in and then to get out for a profit. More or less and impossible task. "
  (:require
    [clojure.test :as tst :refer [is with-test]]
    [clj-http.client :as cnt]
    [clojure.data.json :as jsn]
    [clojurewerkz.envision.core :as envision]
    [clojurewerkz.envision.chart-config :as cfg]
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
    [core.utils :as u]
    [clojure.core.logic :as lgc]
    [stub-http.core :refer :all]))

(def server-ip 
  "Server address for source of orderbook info." 
  (System/getenv "TOTHEMOON_SERVER_IP"))

(def timeformat "YYYY-MM-DDTHH:MM:SS")

(def epsilon (Math/ulp 1.0))

(def cc-historical-sample-data
  "Example historical data for BTC/ETH taken on 22/02/2017" 
  [{:grad 6.5972222222222255E-6, :price 43.1, :unixtimestamp 1.463184E9} 
   {:grad 2.5462962962962914E-5, :price 43.67, :unixtimestamp 1.4632704E9} 
   {:grad -5.844907407407404E-5, :price 45.87, :unixtimestamp 1.4633568E9} 
   {:grad -4.687499999999997E-5, :price 40.82, :unixtimestamp 1.4634432E9} 
   {:grad -2.9166666666666704E-5, :price 36.77, :unixtimestamp 1.4635296E9}])

(def cc-historical-sample-response
  "Sample response from crypto compare takeon on 22/02/2017"
  {:status 200, 
   :headers {"Server" "cloudflare-nginx", 
             "Content-Type" "application/json; charset=utf-8", 
             "Access-Control-Allow-Origin" "*", 
             "Connection" "close", 
             "Transfer-Encoding" "chunked", 
             "CF-RAY" "335015ab6f1520f0-LAX", 
             "Date" "Wed, 22 Feb 2017 05:37:11 GMT", 
             "Cache-Control" "private"}, 
   :body "{\"Response\":\"Success\",\"Message\":\"Battlecruiser operational.Make it happen.Set a course.Take it slow.\",\"Type\":100,\"Aggrgated\":false,\"Data\":[{\"time\":1470441600.0,\"open\":53.76,\"high\":54.11,\"low\":52.36,\"close\":54.11,\"volumefrom\":4.3085315400000006,\"volumeto\":230.9746682058271},{\"time\":1470528000.0,\"open\":54.11,\"high\":54.29,\"low\":51.02,\"close\":54.29,\"volumefrom\":5.5666466000000012,\"volumeto\":293.26111067387626},{\"time\":1470614400.0,\"open\":52.36,\"high\":54.29,\"low\":52.36,\"close\":54.29,\"volumefrom\":3.2970752999999995,\"volumeto\":177.8866873675988}],\"FirstValueInArray\":true,\"TimeTo\":1487721600,\"TimeFrom\":1470441600}", 
   :request-time 734, 
   :trace-redirects ["https://www.cryptocompare.com/api/data/histoday/?aggregate=1&e=CCCAGG&fsym=BTC&limit=200&tsym=ETH"], 
   :orig-content-encoding "gzip", 
   :cookies {"__cfduid" {:discard false, :domain "cryptocompare.com", 
                         :expires #inst "2018-02-22T05:37:11.000-00:00", 
                         :path "/", 
                         :secure false, 
                         :value "d0d3ea2639b89495432ae6af3d3d03c671487741831", 
                         :version 0}}})

(s/def ::limit 
  (s/with-gen 
    (s/and 
      int?
      #(< 0 %)
      #(>= 100 %))  
    (fn [] (s/gen #{1}))))

(s/def ::not-empty-coll #(not (empty? %)))

(s/def ::Quantity number?)

(s/def ::Rate number?)

(s/def ::_id string?)

(s/def ::ob-price-entry (s/keys :req-un [::Quantity ::Rate])) 

(s/def ::orders (s/coll-of ::ob-price-entry :min-count 2))

(s/def ::buy ::orders)

(s/def ::sell ::orders)

(s/def ::orderbook (s/keys :req-un [::buy ::sell]))

(s/def ::ob-db-entry (s/keys :req-un [::_id ::timestamp ::orderbook]))

(s/def ::bidavg number?)

(s/def ::askavg number?)

(s/def ::bidavgs (s/coll-of ::bidavg))

(s/def ::askavgs (s/coll-of ::askavg))

;Should make sure these are in sequential order
(s/def ::timeseries (s/coll-of ::u/timestamp))

(s/def ::unixtimeseries (s/coll-of ::unixtimestamp))

(s/def ::obavg (s/keys :req-un [::bidavg ::askavg]))

(s/def ::timed-avg (s/keys :req-un [::timestamp ::obavg]))

(s/def ::x coll?)

(s/def ::y coll?)

(s/def ::z coll?)

(s/def ::xyplot (s/keys :req-un [::x ::y]))

(s/def ::xyz (s/keys :req-un [::x ::y ::z]))

(s/def ::bidaskavg-timeseries (s/keys :req-un [::bidavgs ::askavgs ::timeseries]))

(s/def ::currency #{"BTC" "NLG"})

(s/def ::Id number?)

(s/def ::TimeStamp ::u/timestamp)

(s/def ::Price number?)

(s/def ::priceseries (s/coll-of ::Price))

(s/def ::Total number?)

(s/def ::FillType string?)

(s/def ::OrderType string?)

(s/def ::rawpricedata 
  (s/keys :req-un [::Id ::TimeStamp ::Quantity ::Price ::Total ::FillType ::OrderType]))

(s/def ::grad number?)

(s/def ::grads (s/coll-of number?))

(s/def ::price number?)

(s/def ::gradentry (s/keys :req-un [::grad ::price ::unixtimestamp]))

(s/def ::grad-timeseries (s/keys :req-un [::grads ::unixtimeseries]))

(s/def ::timestampkey #{:TimeStamp})

(s/def ::unixtimestamp-map (s/keys :req-un [::unixtimestamp ::price]))

(defn zero-guard 
  "Used to protect against divide by zero when calculating gradient, which would give an infinite result. Instead return
  the lowest delta x possible for a unix timestamp, 1. " 
  [x]
  (if (= 0 x) 1 x))

(defn coll= [& colls]
      (apply = (map frequencies colls)))

(s/fdef get-orderbook-info 
        :args (s/cat :limit ::limit) 
        :ret (s/coll-of ::ob-db-entry))

(defn get-orderbook-info 
 "Get orderbook info from our internal store." 
  [limit] 
  (as-> server-ip x
        (cnt/get (str x "/orderbook?limit=" limit)) 
        (:body x)
        (jsn/read-str x :key-fn keyword)))

(s/fdef orderbook-entry-avg  
        :args (s/cat :ob-entry ::orderbook)
        :ret ::obavg)

(defn orderbook-entry-avg 
  "Given a particular entry, get the avg bid and ask"
  [ob-entry]
  {:bidavg (u/average (map :Rate (:buy ob-entry)))
   :askavg (u/average (map :Rate (:sell ob-entry)))})

(s/fdef ob-avgs
  :args (s/cat :orderbooks (s/coll-of ::ob-db-entry :min-count 1))
  :ret (s/coll-of ::timed-avg))

(defn ob-avgs 
  "Returns a timeseries of average bidask prices given a series of orderbooks." 
  [orderbooks]
  (map 
    (fn [y] 
      {:timestamp (:timestamp y)
       :obavg (orderbook-entry-avg (:orderbook y))})
    orderbooks))

(s/fdef ob-avgs->xyz
        :args (s/cat :timedavgs (s/coll-of ::timed-avg)) 
        :ret (s/keys :req-un [::bidavgs ::askavgs ::unixtimeseries]))

(defn ob-avgs->xyz 
 "Separate the orderbook timeseries averages into bid and ask averages" 
  [timedavgs]
  (reduce 
    (fn [x y]
      {:bidavgs (cons (get-in y [:obavg :bidavg]) (:bidavgs x)) 
       :askavgs (cons (get-in y [:obavg :askavg]) (:askavgs x))
       :unixtimeseries (cons 
                         (-> 
                           (:timestamp y) 
                           (u/timestamp->unix (f/formatters :date-time-parser)))
                         (:timeseries x))}) 
    {:bidavgs '() 
     :askavgs '() 
     :unixtimeseries '()}
    timedavgs))

(s/fdef weighted-avg 
        :args (s/cat :orderdata ::orders)
        :ret number?)

(defn weighted-avg 
 "Weighted average of orderbook data." 
  [orderdata]
  (/
    (as-> orderdata x
          (map 
            (fn [{:keys [:Quantity :Rate]}]  
              (* Quantity Rate)) 
            x)
          (reduce + x))
    (as-> orderdata x
          (map :Quantity x)
          (reduce + x))))

(s/fdef price-history-raw 
        :args (s/and (s/cat :basecurrency ::currency :othercurrency ::currency)
                     #(not= (:basecurrency %) (:othercurrency %))
                     #(= (:basecurrency %) "BTC"))
        :ret (s/coll-of ::rawpricedata))

(defn price-history-raw 
  "Raw price data from bittrex." 
  [basecurrency othercurrency]
  (as-> (str 
          "http://bittrex.com/api/v1.1/public/getmarkethistory?market=" 
          basecurrency
          "-" 
          othercurrency
          "&count=50") x
        (cnt/get x) 
        (:body x)
        (jsn/read-str x :key-fn keyword)
        (:result x)))

(s/fdef price-history-timeseries 
        :args (s/and (s/cat :basecurrency ::currency :othercurrency ::currency)
                     #(not= (:basecurrency %) (:othercurrency %))
                     #(= (:basecurrency %) "BTC"))
        :ret (s/keys :req-un [::unixtimeseries ::priceseries])
        :fn (fn [x]
              (let [ret (:ret x)
                    basecurrency (-> x :args :basecurrency) 
                    othercurrency (-> x :args :othercurrency)]
                (coll= 
                  (map 
                    u/timestamp->unix
                    (map :TimeStamp (price-history-raw basecurrency othercurrency)))
                  (:unixtimeseries ret)))))

(defn price-history-timeseries 
 "Price history timeseries, ready for plotting with incanter. History from bittrex." 
  [basecurrency othercurrency]
  (as-> (price-history-raw basecurrency othercurrency) x
        (reduce 
          (fn [y z]
            {:unixtimeseries (cons (u/timestamp->unix (:TimeStamp z)) (:unixtimeseries y))
             :priceseries (cons (:Price z) (:priceseries y))}) 
          {:unixtimeseries '()
           :priceseries '()}
          x)))

(s/fdef grad-calc
        :args (s/cat :raw-history (s/coll-of ::unixtimestamp-map :min-count 2))
        :ret  (s/coll-of ::gradentry)
        :fn (fn [{:keys [args ret]}]
              (->>
                (map :unixtimestamp ret)  
                (apply <=))))

(defn grad-calc 
  "Given time series data, calculates the gradient at each step. The gradient for a particular candle is the difference 
  between its close price and the close price of the next candle, divided by the time difference." 
  [raw-history]
  (-> 
    (reduce
      (fn [x y]
        (cond 
          (vector? x) (let [curr (last x)
                            {:keys [price unixtimestamp]} curr 
                            {nextprice :price nextstamp :unixtimestamp} y]

                        (into (vec (drop-last x)) 
                              [{:grad (/ (- nextprice price) 
                                         (zero-guard (- nextstamp unixtimestamp)))
                                :price price 
                                :unixtimestamp unixtimestamp}
                               {:price nextprice 
                                :unixtimestamp nextstamp}]))

          :else (let [{:keys [price unixtimestamp]}  x
                      {nextprice :price nextstamp :unixtimestamp}  y]

                  [{:grad (/ (- nextprice price) 
                             (zero-guard (- nextstamp unixtimestamp)))
                    :price price 
                    :unixtimestamp unixtimestamp}
                   {:price nextprice 
                    :unixtimestamp nextstamp}])))
      raw-history) 
    drop-last))

(defn map->mapofcol [damap]
  (into {} (map (fn [x] [(first x) [(last x)]]) damap)))

(defn series-insert [series damap]
  (apply 
    merge 
    (map 
      (fn [y]   
        {(first y) (conj (last y) ((first y) damap))})
      series)))

(s/fdef reduce->mapofcol 
        :args (s/cat :collofmap (s/coll-of map?))
        :ret map?
        :fn (fn [{:keys [args ret]}] false))

(defn reduce->mapofcol 
  "Takes a collection of maps and returns a map with the keys of the individual maps, and all of their corresponding 
  values in vectors for each of the keys. Motivation is to be able to plot the data using incanter." 
  [collofmap]
  (reduce 
    (fn [x y]
      (cond 
        (vector? (val (first x))) (series-insert x y) 
        :else (series-insert (map->mapofcol x) y))) 
    collofmap))

(defn update-key 
  "Rename all of the keys in a seq of maps" 
  [amap target replacement]
  (map #(set/rename-keys % {target replacement}) amap))

(defn consecutive-gradient-neg 
  "Takes OHLC+gradient timeseries, the gradient above which the gradient is considered high or part of a spike, an 
  incanter plot and how many consecutive occurances of a gradient above the gradient limit need to occur in a row for 
  it to be considered a spike." 
  [gradz gradlimit plot chain-length]
  (doall 
    (->>
      (reduce 
        (fn [x {:keys [grad] :as y}]
          (cond
            (> gradlimit grad) (update-in 
                                 x 
                                 [(- (count x) 1)]
                                 (fn [a] (conj a y)))
            (< gradlimit grad) (conj x [])
            :else x)) 
        [[]]
        gradz)
      (filter #(<= chain-length (count %)))
      (map last)
      (map #(ich/add-pointer plot (:unixtimestamp %) (:price %))))))

(defn consecutive-gradient-pos 
  "Takes OHLC+gradient timeseries, the gradient above which the gradient is considered high or part of a spike, an 
  incanter plot and how many consecutive occurances of a gradient above the gradient limit need to occur in a row for 
  it to be considered a spike. Returns the incanter plot." 
  [gradz gradlimit plot chain-length]
  (doall 
    (->>
      (reduce 
        (fn [x {:keys [grad] :as y}]
          (cond
            (< gradlimit grad) (update-in 
                                 x 
                                 [(- (count x) 1)]
                                 (fn [a] (conj a y)))
            (> gradlimit grad) (conj x [])
            :else x)) 
        [[]]
        gradz)
      (filter #(<= chain-length (count %)))
      (map last)
      (map 
        #(ich/add-pointer 
           plot 
           (:unixtimestamp %) 
           (:price %))))))

(defn cc-historical-url-gen 
  "Generate valid url for getting historical data from cryptocompare.com" 
  [fsym tsym limit]
  (str "https://www.cryptocompare.com/api/data/histoday/?aggregate=1&e=CCCAGG&fsym=" fsym "&limit=" limit "&tsym=" tsym))

(with-test 

  (defn get-gradients-cc 
    "Get historical data for supplied currency pair from crypto compare and calculate gradients for each candle."  
    [url]
    (as-> url x
          (cnt/get x) 
          (:body x)
          (jsn/read-str x :key-fn keyword)
          (:Data x)
          (update-key x :open :price)
          (update-key x :time :unixtimestamp)
          (grad-calc x))) 

  (with-routes!
    {"/something" cc-historical-sample-response}
    (= 
      (get-gradients-cc (str uri "/something")) 
      '({:grad 4.050925925925942E-6, :price 53.76, :unixtimestamp 1.4704416E9} 
        {:grad -2.025462962962963E-5, :price 54.11, :unixtimestamp 1.470528E9}))))

(defn display-gradient-strat
  "Graphically display the gradient strat. This will add indictors to the chart where the gradient algo returns a 
  positive result. e.g. ![Gradient EA example](http://i.imgur.com/AenlmYI.png)"
  [fsym tsym limit gradlimit chain-length]
  (let [gradz (get-gradients-cc (cc-historical-url-gen fsym tsym limit))
        {:keys [grad price unixtimestamp]} (reduce->mapofcol gradz)
        plot (ich/time-series-plot unixtimestamp price :points true)]
    (consecutive-gradient-pos gradz gradlimit plot chain-length)
    (ic/view plot)))

