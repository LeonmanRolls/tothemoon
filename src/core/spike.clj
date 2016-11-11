(ns core.spike
    (:require
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
      [clojure.core.logic :as lgc]))

(def server-ip "http://139.59.226.3:3001")

(def timeformat "YYYY-MM-DDTHH:MM:SS")

(def epsilon (Math/ulp 1.0))

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
(s/def ::timeseries (s/coll-of ::timestamp))


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

(s/def ::TimeStamp ::timestamp)

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


(defn explain [spec data]
 (s/explain spec data))

(defn generate [spec]
  (gen/generate 
    (s/gen spec)))

(defn zero-guard [x]
  (if (= 0 x) 1 x))

(defn coll= [& colls]
      (apply = (map frequencies colls)))

(s/fdef get-orderbook-info 
        :args (s/cat :limit ::limit) 
        :ret (s/coll-of ::ob-db-entry))

(defn get-orderbook-info [limit] 
  (as-> server-ip x
        (cnt/get (str x "/orderbook?limit=" limit)) 
        (:body x)
        (jsn/read-str x :key-fn keyword)))

(s/fdef avg  
        :args (s/cat :num-col (s/and (s/coll-of number?) #(not (empty? %))))
        :ret number?)

(defn avg [num-col]
  (/
    (reduce + num-col) 
    (count num-col)))

(s/fdef orderbook-entry-avg  
        :args (s/cat :ob-entry ::orderbook)
        :ret ::obavg)

(defn orderbook-entry-avg 
  "Given a particular entry, get the avg bid and ask"
  [ob-entry]
  {:bidavg (avg (map :Rate (:buy ob-entry)))
   :askavg (avg (map :Rate (:sell ob-entry)))})

(s/fdef ob-avgs
  :args (s/cat :orderbooks (s/coll-of ::ob-db-entry :min-count 1))
  :ret (s/coll-of ::timed-avg))

(defn ob-avgs [orderbooks]
  (map 
    (fn [y] 
      {:timestamp (:timestamp y)
       :obavg (orderbook-entry-avg (:orderbook y))})
    orderbooks))


(s/fdef unix->timestamp 
        :args (s/cat :timestamp ::unixtimestamp) 
        :ret (s/and 
               #(not (.contains % "Z"))
               string?))

(defn unix->timestamp [timestamp]
  (clojure.string/join (drop-last 2 (.toString (c/from-long timestamp)))))

(s/fdef ob-avgs->xyz
        :args (s/cat :timedavgs (s/coll-of ::timed-avg)) 
        :ret (s/keys :req-un [::bidavgs ::askavgs ::unixtimeseries]))

(defn ob-avgs->xyz [timedavgs]
  (reduce 
    (fn [x y]
      {:bidavgs (cons (get-in y [:obavg :bidavg]) (:bidavgs x)) 
       :askavgs (cons (get-in y [:obavg :askavg]) (:askavgs x))
       :unixtimeseries (cons 
                         (-> 
                           (:timestamp y) 
                           (timestamp->unix (f/formatters :date-time-parser))) 
                         (:timeseries x))}) 
    {:bidavgs '() 
     :askavgs '() 
     :unixtimeseries '()}
    timedavgs))

(s/fdef offset 
        :args (s/cat :data sequential? :amount number?)
        :ret (s/coll-of number?))

(defn offset [data amount]
  (map #(+ % amount) data))

(s/fdef scale 
        :args (s/cat :data sequential? :amount number?)
        :ret (s/coll-of number?))

(defn scale [data amount]
  (map #(* % amount) data))

(s/fdef weighted-avg 
        :args (s/cat :orderdata ::orders)
        :ret number?)

(defn weighted-avg [orderdata]
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

(defn price-history-raw [basecurrency othercurrency]
  (as-> (str 
          "https://bittrex.com/api/v1.1/public/getmarkethistory?market=" 
          basecurrency
          "-" 
          othercurrency
          "&depth=50") x
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
                    timestamp->unix 
                    (map :TimeStamp (price-history-raw basecurrency othercurrency)))
                  (:unixtimeseries ret)))))

(defn price-history-timeseries [basecurrency othercurrency]
  (as-> (price-history-raw basecurrency othercurrency) x
        (reduce 
          (fn [y z]
            {:unixtimeseries (cons (timestamp->unix (:TimeStamp z)) (:unixtimeseries y))
             :priceseries (cons (:Price z) (:priceseries y))}) 
          {:unixtimeseries '()
           :priceseries '()}
          x)))

(s/fdef bid-ask-avg-timeseries 
        :args (s/cat :limit ::limit)
        :ret (s/keys :req-un [::bidavgs ::askavgs ::unixtimeseries]))

(defn bid-ask-avg-timeseries [limit] 
  (as-> limit x
        (get-orderbook-info x)
        (reduce 
          (fn [x y]
            {:bidavgs (cons (weighted-avg (get-in y [:orderbook :buy])) (:bidavgs x)) 
             :askavgs (cons (weighted-avg (get-in y [:orderbook :sell])) (:askavgs x))
             :unixtimeseries (cons 
                               (-> 
                                 (:timestamp y) 
                                 (timestamp->unix (f/formatters :date-time-parser))) 
                               (:timeseries x))}) 
          {:bidavgs '() 
           :askavgs '() 
           :unixtimeseries '()}
          x)))

(s/fdef graddata->timeseries 
        :args (s/cat :raw-history (s/coll-of ::gradentry))
        :ret  ::grad-timeseries)

(defn graddata->timeseries [grad-data]
  (reduce 
    (fn [x y]
      {:grads (cons (:grad y) (:grads x))
       :unixtimeseries (cons (:unixtimestamp y) (:unixtimeseries x))}) 
    {:grads '() 
     :unixtimeseries '()}
    grad-data))

(defn colltimestamp->unix [datacoll]
  (map 
    #(update-in % [:TimeStamp] timestamp->unix) 
    datacoll))

(s/fdef grad-calc
        :args (s/cat :raw-history (s/coll-of ::unixtimestamp-map :min-count 2))
        :ret  (s/coll-of ::gradentry)
        :fn (fn [{:keys [args ret]}]
              (->>
                (map :unixtimestamp ret)  
                (apply <=))))

(defn grad-calc [raw-history]
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
        :fn (fn [{:keys [args ret]}]
              false
              ) 
        )

(defn reduce->mapofcol [collofmap]
  (reduce 
    (fn [x y]
      (cond 
        (vector? (val (first x))) (series-insert x y) 
        :else (series-insert (map->mapofcol x) y))) 
    collofmap))

(s/fdef raw->unix 
        :args (s/cat :map (s/coll-of ::rawpricedata :min-count 2) :key ::timestampkey)
        :ret coll? 
        :fn (fn [{:keys [args ret]}]
              (->>
                (map :unixtimestamp ret)  
                (apply <=))))

(defn raw->unix [raw timestampkey]
  (->> 
    (map 
      (fn [x]
        (update-in x [timestampkey] timestamp->unix))
      raw)
    (map #(set/rename-keys % {timestampkey :unixtimestamp})) 
    (sort-by :unixtimestamp)))

(defn update-key [amap target replacement]
  (map #(set/rename-keys % {target replacement}) amap))

(defn consecutive-gradient-neg [gradz gradlimit plot chain-length]
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

(defn consecutive-gradient-pos [gradz gradlimit plot chain-length]
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

(comment 

  (let [gradz (as-> (str "https://www.cryptocompare.com/api/data/histoday/?aggregate=1&e=CCCAGG&fsym=ETH&limit=1000&tsym=BTC") x
                    (cnt/get x) 
                    (:body x)
                    (jsn/read-str x :key-fn keyword)
                    (:Data x)
                    (update-key x :open :price)
                    (update-key x :time :unixtimestamp)
                    (grad-calc x))
        {:keys [grad price unixtimestamp]} (reduce->mapofcol gradz)
        plot (ich/time-series-plot  
               unixtimestamp
               price
               :points true)
        ]


(consecutive-gradient-pos gradz 0.00000001 plot 1)
    (ic/view plot)

    )

    (doall 
      (map 
        #(ich/add-pointer plot (:unixtimestamp %) (:price %))  
        highest-grads))

  (ic/view 
    (ich/xy-plot  
      unixtimestamp
      price
      :points true))

  (def sorted-vec (gen/fmap sort (gen/vector (gen/int))))
  (gen/sample sorted-vec)

  (s/def ::lookup (s/map-of keyword? string? :min-count 1))

  (s/def ::lookup-finding-k (s/and (s/cat :lookup ::lookup
                                          :k keyword?)
                                   (fn [{:keys [lookup k]}]
                                     (contains? lookup k))))
  (defn lookup-finding-k-gen []
    (gen/bind 
      (s/gen ::lookup) 
      #(gen/tuple 
         (gen/return %) 
         (gen/elements (keys %)))))

  )

