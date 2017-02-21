(ns core.utils
  (:require
    [clj-http.client :as cnt]
    [clojure.data.json :as jsn]
    [clojure.spec :as s]
    [clojure.spec.gen :as gen]
    [clojure.spec.test :as ts :refer [check]]
    [clojure.spec.gen :as gen]
    [clj-time.format :as f]
    [clj-time.predicates :as pr]
    [clojure.test :as tst :refer [is with-test]]
    [clojure.core.async :as casy :refer [<!! <! >! go chan go-loop timeout]]
    [clj-time.coerce :as c]
    [stub-http.core :refer :all]))

(def cmc-syms 
 "Coins available from coinmarketcap.com"  
  #{"bitcoin" "ethereum" "ripple" "litecoin" "monero" "ethereum-classic" "dash" "augur" "maidsafecoin" "waves" "nem" "steem" "dogecoin" "factom" "digixdao" "lisk" "gulden" "stellar" "bitshares" "shadowcoin" "peerplays" "iconomi" "ardor" "gamecredits" "bytecoin-bcn" "storjcoin-x" "xaurum" "antshares" "emercoin" "counterparty" "singulardtv" "tether" "siacoin" "nxt" "synereo" "bitcrystals" "stratis" "peercoin" "agoras-tokens" "ybcoin" "iocoin" "vcash" "bitcoindark" "namecoin" "syscoin" "rubycoin" "global-currency-reserve" "zcash" "potcoin" "nav-coin" "digibyte" "blackcoin" "yocoin" "solarcoin" "omni" "gridcoin" "supernet-unity" "decred" "scotcoin" "steem-dollars" "fuelcoin" "sarcoin" "nautiluscoin" "vpncoin" "faircoin" "expanse" "curecoin" "fedoracoin" "earthcoin" "swiscoin" "monacoin" "clams" "auroracoin" "startcoin" "digitalnote" "primecoin" "nexus" "burst" "asiadigicoin" "breakout-stake" "reddcoin" "radium" "vertcoin" "worldcoin" "vericoin" "feathercoin" "hitcoin" "qora" "blocknet" "boolberry" "quark" "i0coin" "dnotes" "hicoin" "mmnxt" "nubits" "darknet" "goldcoin" "library-credit" "novacoin" "breakout" "boostcoin" "mintcoin" "bitbay" "obits" "virtacoin" "gambit" "play" "diamond" "safe-exchange-coin" "adzcoin" "aeon" "project-decorum" "bilshares" "megacoin" "blockpay" "zetacoin" "triggers" "zeitcoin" "casinocoin" "europecoin" "stealthcoin" "viacoin" "nushares" "wild-beast-block" "florincoin" "infinitecoin" "riecoin" "cloakcoin" "zcoin" "unobtanium" "zccoin" "rise" "myriad" "pesobit" "silk" "salus" "ambercoin" "applecoin" "voxels" "digitalcoin" "circuits-of-value" "evergreencoin" "foldingcoin" "vootcoin" "bitmark" "bitcny" "next-horizon" "okcash" "cannabiscoin" "anoncoin" "verge" "unioncoin" "energycoin" "noblecoin" "cryptonite" "britcoin" "orbitcoin" "gems" "pinkcoin" "coinmarketscoin" "neoscoin" "einsteinium" "electronic-gulden" "smileycoin" "shift" "pepe-cash" "huntercoin" "geocoin" "tagcoin" "audiocoin" "jewels" "diem" "ltbcoin" "mazacoin" "levocoin" "hempcoin-hmp" "securecoin" "fantomcoin" "crowncoin" "trumpcoin" "1credit" "spreadcoin" "dubaicoin" "stabilityshares" "bitcoin-plus" "gycoin" "stress" "vtorrent" "ixcoin" "sync" "maxcoin" "woodcoin" "capricoin" "skynet-asset" "sibcoin" "bellacoin" "bitstar" "mooncoin" "magi" "coin2-1" "mineum" "synergy" "pangea-poker" "creditbit" "groestlcoin" "bitswift" "bitusd" "netcoin" "swing" "veriumreserve" "quatloo" "krypton" "hempcoin" "bytecent" "whitecoin" "qibuck" "joincoin" "dashcoin" "bitsend" "dopecoin" "cryptogenic-bullion" "uro" "bitbean" "applebyte" "trustplus" "rimbit" "sterlingcoin" "blitzcash" "cryptographic-anomaly" "incakoin" "tickets" "dotcoin" "xiaomicoin" "monetaryunit" "titcoin" "pipcoin" "rubies" "cannacoin" "syndicate" "pakcoin" "terracoin" "nxtventure" "cryptofund" "coin" "canada-ecoin" "devcoin" "elcoin-el" "billarycoin" "debune" "the-viral-exchange" "eccoin" "goldpieces" "truckcoin" "dimecoin" "teslacoin" "bluecoin" "dt-token" "deutsche-emark" "pesetacoin" "korecoin" "sphere" "parkbyte" "hodlcoin" "hyper" "sativacoin" "transfercoin" "bitbtc" "1337" "tao" "cryptcoin" "cryptojacks" "postcoin" "unbreakablecoin" "bata" "piggycoin" "sexcoin" "artex-coin" "wexcoin" "secretcoin" "goldblocks" "fluttercoin" "moin" "karbowanec" "metalcoin" "sling" "mojocoin" "arcticcoin" "blakecoin" "hobonickels" "grantcoin" "memetic" "influxcoin" "exclusivecoin" "archcoin" "bottlecaps" "veltor" "franko" "hyperstake" "tekcoin" "newbium" "universal-currency" "vip-tokens" "soilcoin" "gpu-coin" "bitseeds" "mastertradercoin" "globalboost-y" "bbqcoin" "ziftrcoin" "x-coin" "nolimitcoin" "digicube" "granitecoin" "francs" "8bit" "gapcoin" "fujicoin" "cryptoescudo" "primechain" "neutron" "steps" "songcoin" "hommalicoin" "apexcoin" "dollarcoin" "gcoin" "ucoin" "alexium" "destiny" "spacecoin" "bios-crypto" "unitus" "joulecoin" "berncash" "beatcoin" "bipcoin" "42-coin" "warp" "prime-xi" "letitride" "rhinocoin-rhc" "firecoin" "allsafe" "insanecoin" "bikercoin" "zonecoin" "sixeleven" "gamebet-coin" "gamerholiccoin" "bolivarcoin" "chronos" "genstake" "zayedcoin" "crevacoin" "evil-coin" "islacoin" "anarchistsprime" "litebar" "jobscoin" "impulsecoin" "guccionecoin" "bantam" "agrolifecoin" "emirates-gold-coin" "sydpak" "808coin" "xp" "cannabis-industry-coin" "unrealcoin" "powercoin" "world-gold-coin" "mudracoin" "trmb" "kilocoin" "ion" "bitshares-music" "jinn" "instantdex" "ico-openledger" "neucoin" "htmlcoin" "xcurrency" "asiacoin" "2give" "colossuscoin-v2" "btsr" "btctalkcoin" "librexcoin" "ultracoin" "leafcoin" "flycoin" "coinomat" "mediterraneancoin" "pandacoin-pnd" "liquid" "rare-pepe-party" "bigup" "kobocoin" "nxttycoin" "bitcointx" "sooncoin" "litedoge" "paycoin2" "sprouts" "the-cypherfunks" "tilecoin" "yacoin" "lottocoin" "wayguide" "swagbucks" "martexcoin" "putincoin" "quazarcoin" "checkcoin" "quotient" "trollcoin" "reecoin" "supercoin" "sproutsextreme" "globalcoin" "bitsilver" "gaia" "plncoin" "triangles" "ratecoin" "smartcoin" "bitbar" "ufo-coin" "arbit" "qubitcoin" "bitz" "nyancoin" "bitgold" "hamradiocoin" "viral" "datacoin" "satoshimadness" "mindcoin" "amsterdamcoin" "freicoin" "aricoin" "tigercoin" "cashout" "russiacoin" "petrodollar" "redcoin" "cypher" "emerald" "aurumcoin" "octocoin" "tittiecoin" "cagecoin" "crypto" "guncoin" "bunnycoin" "fastcoin" "beavercoin" "argentum" "philosopher-stones" "bumbacoin" "coexistcoin" "revolvercoin" "hundredcoin" "phoenixcoin" "biteur" "bitcoin-scrypt" "mgw" "cubits" "uniqredit" "halcyon" "bitzeny" "antibitcoin" "debitcoin" "flavorcoin" "vaperscoin" "kittehcoin" "leacoin" "ronpaulcoin" "prototanium" "dobbscoin" "captcoin" "spots" "icash" "mangocoinz" "dappster" "atomic-coin" "paycon" "cybercoin" "evotion" "wmcoin" "osmiumcoin" "breakcoin" "zurcoin" "ego" "metal-music-coin" "mmbtcd" "floz" "chesscoin" "pospro" "blazecoin" "vcoin" "bitcloud" "topcoin" "bitcoin-21" "popularcoin" "lanacoin" "guarany" "high-voltage" "acoin" "independent-money-system" "unicoin" "digitalprice" "b3coin" "jin-coin" "newyorkcoin" "elementrem" "parallelcoin" "orlycoin" "fuzzballs" "elcoin" "corgicoin" "nevacoin" "pulse" "machinecoin" "posex" "px" "helleniccoin" "c-bit" "mustangcoin" "cabbage" "unfed" "xonecoin" "comet" "eurocoin" "dpay" "tagrcoin" "virtualcoin" "bitquark" "revenu" "chaincoin" "selfiecoin" "cryptbit" "photon" "money" "cashcoin" "shilling" "bittokens" "bowscoin" "tajcoin" "ponzicoin" "stronghands" "antilitecoin" "litecred" "save-and-gain" "imperialcoin" "number7" "p7coin" "batcoin" "swaptoken" "crtcoin" "enigma" "californium" "digital-credits" "pizzacoin" "khancoin" "ernus" "23-skidoo" "satoshicard" "horiemoncard" "royalcoin-2" "nxttyacci" "forevercoin" "pluton" "e-dinar-coin" "bfx" "leocoin" "techshares" "clubcoin" "maskcoin" "wowecoin" "dynamiccoin" "asset-backed-coin" "heat-ledger" "the-dao" "edrcoin" "index-coin" "omicron" "mind-gene" "first-blood" "alpacoin" "npccoin" "gaycoin" "biglifecoin" "international-diamond" "uncoin" "tbcoin" "happy-creator-coin" "caliphcoin" "first-bitcoin" "invisiblecoin" "deltacredits" "gbcgoldcoin" "taopay" "timekoin" "futurepoints" "lecoin" "neptunecoin" "alphabet-coin-fund" "rhodiumcoin" "lomocoin" "bitland" "eclipse" "digitalfund" "enecoin" "kolschcoin" "sharkcoin" "bagcoin" "revcoin" "clinton" "president-johnson" "woodshares" "cthulhu-offerings" "gotfomo" "peacecoin" "cartercoin" "xaucoin" "shellpay" "advanced-internet-blocks" "royalcoin" "president-trump" "upcoin" "eggcoin" "futcoin" "rcoin" "goldmaxcoin" "cbd-crystals" "chncoin" "cleverbot" "bitalphacoin" "ocow" "trickycoin" "frankywillcoin" "psilocybin" "bitcoinfast" "rublebit" "kcoin" "local-family-owned" "richcoin" "digital-bullion-gold" "gameleaguecoin" "fedorashare" "pabyosicoin" "denarius" "flaxscript" "braincoin" "asiccoin" "avatarcoin" "xab" "motocoin" "todaycoin" "quebecoin" "cycling-coin" "linkedcoin" "dubstep" "operand" "opescoin" "mobilecash" "lazaruscoin" "sportscoin" "darklisk" "superstaketoken" "skeincoin" "president-clinton" "ugain" "lathaan" "fitcoin" "vegascoin" "sakuracoin" "paypeer" "moneta2" "digieuro" "prismchain" "pokechain" "teamup" "aces" "cashme" "bitmoon" "golfcoin" "nucleustokens" "valorbit" "superturbostake" "thecreed" "fireflycoin" "x2" "soulcoin" "safecoin" "pokecoin" "tellurion" "victoriouscoin" "ganjacoin-v2" "choofcoin" "xpay" "nutcoin" "paccoin"})

(def oanda-account-rep
  "Local representation of most important oanda account details" 
  {:current-order {} :open-date nil :stop-loss nil :buy-or-sell nil :order-price nil :account 1000 :history []})

(s/def ::current-order any?)
(s/def ::open-date any?)
(s/def ::stop-loss any?)
(s/def ::buy-or-sell any?)
(s/def ::order-price any?)
(s/def ::account any?)
(s/def ::history any?)

(s/def ::mock-account 
   (s/keys :req-un [::open-date ::stop-loss ::buy-or-sell ::order-price ::account ::history]))

(s/def ::url (s/with-gen
               (s/and string? #(.contains % "://"))
               (fn [] (s/gen #{"https://www.cryptocompare.com/api/data/coinlist/"}))))

(s/def ::unixtimestamp
  (s/with-gen
    (s/and number? #(> 1577580878000 % 0))
    (fn [] (s/gen #{157758087000}))))

(s/def ::coinmarketcap-sym cmc-syms)

(s/def ::price double?)

(s/def ::open double?)
(s/def ::close double?)
(s/def ::high double?)
(s/def ::low double?)

(s/def ::standard-candle (s/keys :req-un [::open ::close ::high ::low ::unixtimestamp]))

(s/def ::formatter
  (s/with-gen
    #(= (type %) org.joda.time.format.DateTimeFormatter)
    (fn [] (s/gen #{(f/formatters :date-time-parser)}))))

(s/def ::timestamp
  (s/with-gen
    (s/and
      string?
      #(.contains % "T"))
    (fn []
        (gen/fmap
          #(.toString (c/from-date %))
          (s/gen (s/inst-in #inst "2015" #inst "2016"))))))

;##Basic candle sequences for simple strat
;
;Sequences of candles primarily for testing. 'stop-out' means that the last candle would trigger the stop loss placed 
;based on the previous candle according to simple strat. Values below are the overall profit or loss after the whole 
;sequence has been run through simple strat.  

(def green->green 
  "0.1 profit"
  [{:unixtimestamp 1487304000000, :open 1.00000, :low 0.90000, :high 1.20000, :close 1.10000}
   {:unixtimestamp 1487307600000, :open 1.10000, :low 1.00000, :high 1.30000, :close 1.20000}])

(def green->red 
  "0.1 loss"
  [{:open 1.2 :high 1.5 :low 1.1 :close 1.3 :unixtimestamp 1479356762}
   {:open 1.3 :high 1.4 :low 1.15 :close 1.20 :unixtimestamp 1479366763}])

(def green->green->green-stop-out 
  "0.1 loss"
  [{:open 1 :high 1.3 :low 0.9 :close 1.2 :unixtimestamp 1479347761}
   {:open 1.2 :high 1.4 :low 1.1 :close 1.3 :unixtimestamp 1479358762}
   {:open 1.3 :high 1.5 :low 1 :close 1.4 :unixtimestamp 1479369763}])

(def green->green->red 
  "Break even"
  [{:open 1 :high 1.3 :low 0.9 :close 1.2 :unixtimestamp 1479346761}
   {:open 1.2 :high 1.4 :low 1.1 :close 1.3 :unixtimestamp 1479356762}
   {:open 1.3 :high 1.5 :low 1.15 :close 1.20 :unixtimestamp 1479366763}])

(def red->red->red-stop-out 
  "0.1 loss"
  [{:open 1 :high 1.1 :low 0.7 :close 0.8 :unixtimestamp 1479346761}
   {:open 0.8 :high 0.9 :low 0.6 :close 0.7 :unixtimestamp 1479356762}
   {:open 0.7 :high 1 :low 0.5 :close 0.6 :unixtimestamp 1479366763}])

(def red->red->green 
  "Break even"
  [{:open 1 :high 1.1 :low 0.7 :close 0.8 :unixtimestamp 1479346761}
   {:open 0.8 :high 0.9 :low 0.6 :close 0.7 :unixtimestamp 1479356762}
   {:open 0.7 :high 0.85 :low 0.5 :close 0.8 :unixtimestamp 1479366763}])

(def red->green-stop-out->green->red 
  "0.1 loss" 
  [{:open 1 :high 1.1 :low 0.8 :close 0.9 :unixtimestamp 1479346761}
   {:open 0.9 :high 1.2 :low 0.8 :close 1.1 :unixtimestamp 1479356762}
   {:open 1.1 :high 1.4 :low 1 :close 1.3 :unixtimestamp 1479366763}
   {:open 1.3 :high 1.4 :low 1.1 :close 1.2 :unixtimestamp 1479376763}])

(def green->red->red->green 
  "0.2 loss" 
  [{:open 0.9 :high 1.2 :low 0.8 :close 1.1 :unixtimestamp 1479346761}
   {:open 1.1 :high 1.2 :low 0.9 :close 1 :unixtimestamp 1479356762}
   {:open 1 :high 1.1 :low 0.7 :close 0.8 :unixtimestamp 1479366763}
   {:open 0.8 :high 1 :low 0.7 :close 0.9 :unixtimestamp 1479376763}])

;## Candle ratios
;
;Some stratergies are based on the ratio between (- close open) and (- high low). In other words the ratio between the 
;'body' of the candle and the difference between the extent of the 'wicks'. Here are some candle patterns with known 
;ratios bewteen the bodies and the wicks.

(def green-half-ratio 
    {:unixtimestamp 1479470400000, :open 1, :low 0.9, :high 1.2, :close 1.1})
(def green-one-ratio 
    {:unixtimestamp 1479470400000, :open 1, :low 0.9, :high 1.3, :close 1.2})
(def green-onehalf-ratio 
  {:unixtimestamp 1479470400000, :open 1, :low 0.9, :high 1.4, :close 1.3})

(def red-half-ratio 
  {:unixtimestamp 1479470400000, :open 1, :low 0.8, :high 1.1, :close 0.9})
(def red-one-ratio 
  {:unixtimestamp 1479470400000, :open 1, :low 0.7, :high 1.1, :close 0.8})
(def red-onehalf-ratio 
  {:unixtimestamp 1479470400000, :open 1, :low 0.6, :high 1.1, :close 0.7})

;## Assorted utilities

(s/fdef json-get
        :args (s/cat :url ::url :opts (s/? map?))
        :ret map?)

(with-test 

  (defn json-get 
    "GET from a JSON source and return a clojure map" 
    [url & opts]
    (as-> (cnt/get url (when opts (first opts))) x
          (:body x)
          (jsn/read-str x :key-fn keyword))) 

  (is 
    (map? (json-get "http://ip.jsontest.com/"))))

(with-test 

  (defn digit-count 
    "Count the number of digits in a number. Doesn't count digits after the decimal place."
    [no]
    (int (+ 1 (Math/floor (Math/log10 no))))) 

  (is 
    (= 5 (digit-count 12345))))

(s/fdef to-human
        :args (s/cat :unix ::unixtimestamp))

(with-test 

  (defn to-human 
    "From unix to human readable timestamp of the form e.g. 2016-11-18T12:00:00.000Z"
    [unix]
    (let [ts-length (digit-count unix)
          convert (fn [x] (.toString (c/from-long (long x))))]
      (cond
        (>= 10 ts-length) (convert (* 1000 unix))
        (< 10 ts-length) (convert unix)
        :else (convert unix))))

  (is 
    (= (to-human 1479470400000) "2016-11-18T12:00:00.000Z")))

(with-test 

  (defn map->mapofcol 
    "Puts the vals of a map into vectors" 
    [damap]
    (into {} (map (fn [x] [(first x) [(last x)]]) damap))) 

  (is 
    (=  
      {:unixtimestamp [1479470400000], :open [1], :low [0.6], :high [1.1], :close [0.7]}
      (map->mapofcol {:unixtimestamp 1479470400000, :open 1, :low 0.6, :high 1.1, :close 0.7}))))

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

(s/fdef average
        ::args (s/cat :numbers (s/? (s/coll-of number?))))

(defn average
      [& numbers]
      (/ (apply + numbers) (count numbers)))

(s/fdef timestamp->unix
        :args (s/cat :timestamp ::timestamp :formatter (s/? ::formatter))
        :ret number?)

(defn timestamp->unix
      ([timestamp]
        (timestamp->unix timestamp (f/formatters :date-time-parser)))

      ([timestamp formatter]
        (as-> timestamp  x
              (f/parse formatter x)
              (c/to-long x))))

(defn update-all-vals [data keyvec fn]
      (map #(update-in % keyvec fn) data))

(defn rename-all-keys [data from to]
      (map #(clojure.set/rename-keys % {from to}) data))

(defn candles-between
      "2014-08-21T00:00:00.000Z - 2014-09-30T00:00:00.000Z"
      [oldest newest candles]
      (filter
        #(< (timestamp->unix oldest)
            (* 1000 (:unixtimestamp %))
            (timestamp->unix newest))
        candles))

(defn first-and-last [coll]
      [(first coll) (last coll)])

(defn green? [{:keys [open close]}]
      (> close open))

(with-test
  (defn percentage-change [old new & opts]
        (let [basic (* (/ (- new old) old) 100)]
             (if opts
               (if (> basic 0)
                 (/ (+ 100 basic) 100)
                 (- 1 (Math/abs (/ basic 100))))
               basic)))

  (is (= 100 (percentage-change 1 2)))
  (is (= -50 (percentage-change 2 1)))
  (is (= 2 (percentage-change 1 2 :heyhey)))
  (is (= 0.9 (percentage-change 1 0.9 :heyhey))))


(defn mid-week? [unixtime]
        (or
          (pr/tuesday? (c/from-long unixtime))
          (pr/wednesday? (c/from-long unixtime))
          (pr/thursday? (c/from-long unixtime))))

(defn vec-remove
      "remove elem in coll"
      [coll pos]
      (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn index-exclude [r ex]
      "Take all indices execpted ex"
      (filter #(not (ex %)) (range r)))

(defn dissoc-idx [v & ds]
      (map v (index-exclude (count v) (into #{} ds))))

(with-test
  (defn body-ratio [{:keys [open low high close] :as candle}]
        (cond
          (green? candle) (/ (- close open) (if (= 0.0 (+ (- high close) (- open low))) 0.001 (+ (- high close) (- open low)) ))
          :red (/ (- open close) (if (= 0.0 (+ (- high open) (- close low))) 0.001 (+ (- high open) (- close low))))))

  (is
    (=
      (/ 0.1 0.2)
      (float (body-ratio green-half-ratio))))
  (is
    (=
      (/ 0.2 0.2)
      (float (body-ratio green-one-ratio))))
  (is
    (=
      (float (/ 0.3 0.2))
      (float (body-ratio green-onehalf-ratio))))
  (is
    (=
      (float (/ 0.1 0.2))
      (float (body-ratio red-half-ratio))))
  (is
    (=
      (float (/ 0.2 0.2))
      (float (body-ratio red-one-ratio))))
  (is
    (=
      (float (/ 0.3 0.2))
      (float (body-ratio red-onehalf-ratio)))))

(defn h1-to-weeks [oanda-data]
      (reduce
        (fn [x y]
            (cond
              (empty? (last x)) (update-in x [0] #(conj % y))
              (< (- (:unixtimestamp y) (:unixtimestamp (last (last x)))) 7200000) (update-in x [(- (count x) 1)] #(conj % y))
              :greater-than-hour (conj x [y])
              ))
        [[]]
        oanda-data))

(defn tue-thu-filter [dadata]
        (->>
          dadata
          vec
          (reduce
            (fn [x y]
                (cond
                  (mid-week? (:unixtimestamp y)) (update-in x [(- (count x) 1)] #(conj % y))
                  :else (conj x [])))
            [[]])
          (filter #(not (empty? %)))))

(defn now [] (new java.util.Date))

(defn on-the-x-second [chan second]
      (go
        (while true
               (<! (timeout 1000))
               (when (= second (.getSeconds (now))) (>! chan true)))))

(defn on-the-minute-second [chan minutes seconds]
      (go
        (while true
               (<! (timeout 1000))
               (when
                 (and
                   (.contains minutes (.getMinutes (now)))
                   (.contains seconds (.getSeconds (now))))
                 (>! chan true)))))

(defn round2
    "Round a double to the given precision (number of significant digits)"
    [precision d]
    (let [factor (Math/pow 10 precision)]
      (/ (Math/round (* d factor)) factor)))

(defn basic-stub
    "Takes a function that executes a request when given a url and returns the result on a channel." 
    [req-fn] 
    (with-routes!
      {"/something" {:status 200 :content-type "application/json"
                     :body   (jsn/write-str {:hello "world"})}}
      (:hello (<!! (req-fn (str uri "/something"))))))

;Mock request as provided for by stub-http
  (defn mock-req->body-data
    "Takes a mock request and returns the body of the response as a map" 
    [mock-req]
    (jsn/read-str (get (:body mock-req) "postData") :key-fn keyword ))

