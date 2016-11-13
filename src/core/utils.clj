(ns core.utils
    (:require
      [clj-http.client :as cnt]
      [clojure.data.json :as jsn]
      [clojure.spec :as s]
      [clojure.spec.gen :as gen]
      [clojure.spec.test :as ts :refer [check]]
      [clojure.spec.gen :as gen]
      [clj-time.format :as f]
      [clj-time.coerce :as c]))

(s/def ::url (s/with-gen
               (s/and string? #(.contains % "://"))
               (fn [] (s/gen #{"https://www.cryptocompare.com/api/data/coinlist/"}))))

(s/def ::unixtimestamp
  (s/with-gen
    (s/and number? #(> 1577580878000 % 0))
    (fn [] (s/gen #{157758087000}))))

(def cmc-syms #{"bitcoin" "ethereum" "ripple" "litecoin" "monero" "ethereum-classic" "dash" "augur" "maidsafecoin" "waves" "nem" "steem" "dogecoin" "factom" "digixdao" "lisk" "gulden" "stellar" "bitshares" "shadowcoin" "peerplays" "iconomi" "ardor" "gamecredits" "bytecoin-bcn" "storjcoin-x" "xaurum" "antshares" "emercoin" "counterparty" "singulardtv" "tether" "siacoin" "nxt" "synereo" "bitcrystals" "stratis" "peercoin" "agoras-tokens" "ybcoin" "iocoin" "vcash" "bitcoindark" "namecoin" "syscoin" "rubycoin" "global-currency-reserve" "zcash" "potcoin" "nav-coin" "digibyte" "blackcoin" "yocoin" "solarcoin" "omni" "gridcoin" "supernet-unity" "decred" "scotcoin" "steem-dollars" "fuelcoin" "sarcoin" "nautiluscoin" "vpncoin" "faircoin" "expanse" "curecoin" "fedoracoin" "earthcoin" "swiscoin" "monacoin" "clams" "auroracoin" "startcoin" "digitalnote" "primecoin" "nexus" "burst" "asiadigicoin" "breakout-stake" "reddcoin" "radium" "vertcoin" "worldcoin" "vericoin" "feathercoin" "hitcoin" "qora" "blocknet" "boolberry" "quark" "i0coin" "dnotes" "hicoin" "mmnxt" "nubits" "darknet" "goldcoin" "library-credit" "novacoin" "breakout" "boostcoin" "mintcoin" "bitbay" "obits" "virtacoin" "gambit" "play" "diamond" "safe-exchange-coin" "adzcoin" "aeon" "project-decorum" "bilshares" "megacoin" "blockpay" "zetacoin" "triggers" "zeitcoin" "casinocoin" "europecoin" "stealthcoin" "viacoin" "nushares" "wild-beast-block" "florincoin" "infinitecoin" "riecoin" "cloakcoin" "zcoin" "unobtanium" "zccoin" "rise" "myriad" "pesobit" "silk" "salus" "ambercoin" "applecoin" "voxels" "digitalcoin" "circuits-of-value" "evergreencoin" "foldingcoin" "vootcoin" "bitmark" "bitcny" "next-horizon" "okcash" "cannabiscoin" "anoncoin" "verge" "unioncoin" "energycoin" "noblecoin" "cryptonite" "britcoin" "orbitcoin" "gems" "pinkcoin" "coinmarketscoin" "neoscoin" "einsteinium" "electronic-gulden" "smileycoin" "shift" "pepe-cash" "huntercoin" "geocoin" "tagcoin" "audiocoin" "jewels" "diem" "ltbcoin" "mazacoin" "levocoin" "hempcoin-hmp" "securecoin" "fantomcoin" "crowncoin" "trumpcoin" "1credit" "spreadcoin" "dubaicoin" "stabilityshares" "bitcoin-plus" "gycoin" "stress" "vtorrent" "ixcoin" "sync" "maxcoin" "woodcoin" "capricoin" "skynet-asset" "sibcoin" "bellacoin" "bitstar" "mooncoin" "magi" "coin2-1" "mineum" "synergy" "pangea-poker" "creditbit" "groestlcoin" "bitswift" "bitusd" "netcoin" "swing" "veriumreserve" "quatloo" "krypton" "hempcoin" "bytecent" "whitecoin" "qibuck" "joincoin" "dashcoin" "bitsend" "dopecoin" "cryptogenic-bullion" "uro" "bitbean" "applebyte" "trustplus" "rimbit" "sterlingcoin" "blitzcash" "cryptographic-anomaly" "incakoin" "tickets" "dotcoin" "xiaomicoin" "monetaryunit" "titcoin" "pipcoin" "rubies" "cannacoin" "syndicate" "pakcoin" "terracoin" "nxtventure" "cryptofund" "coin" "canada-ecoin" "devcoin" "elcoin-el" "billarycoin" "debune" "the-viral-exchange" "eccoin" "goldpieces" "truckcoin" "dimecoin" "teslacoin" "bluecoin" "dt-token" "deutsche-emark" "pesetacoin" "korecoin" "sphere" "parkbyte" "hodlcoin" "hyper" "sativacoin" "transfercoin" "bitbtc" "1337" "tao" "cryptcoin" "cryptojacks" "postcoin" "unbreakablecoin" "bata" "piggycoin" "sexcoin" "artex-coin" "wexcoin" "secretcoin" "goldblocks" "fluttercoin" "moin" "karbowanec" "metalcoin" "sling" "mojocoin" "arcticcoin" "blakecoin" "hobonickels" "grantcoin" "memetic" "influxcoin" "exclusivecoin" "archcoin" "bottlecaps" "veltor" "franko" "hyperstake" "tekcoin" "newbium" "universal-currency" "vip-tokens" "soilcoin" "gpu-coin" "bitseeds" "mastertradercoin" "globalboost-y" "bbqcoin" "ziftrcoin" "x-coin" "nolimitcoin" "digicube" "granitecoin" "francs" "8bit" "gapcoin" "fujicoin" "cryptoescudo" "primechain" "neutron" "steps" "songcoin" "hommalicoin" "apexcoin" "dollarcoin" "gcoin" "ucoin" "alexium" "destiny" "spacecoin" "bios-crypto" "unitus" "joulecoin" "berncash" "beatcoin" "bipcoin" "42-coin" "warp" "prime-xi" "letitride" "rhinocoin-rhc" "firecoin" "allsafe" "insanecoin" "bikercoin" "zonecoin" "sixeleven" "gamebet-coin" "gamerholiccoin" "bolivarcoin" "chronos" "genstake" "zayedcoin" "crevacoin" "evil-coin" "islacoin" "anarchistsprime" "litebar" "jobscoin" "impulsecoin" "guccionecoin" "bantam" "agrolifecoin" "emirates-gold-coin" "sydpak" "808coin" "xp" "cannabis-industry-coin" "unrealcoin" "powercoin" "world-gold-coin" "mudracoin" "trmb" "kilocoin" "ion" "bitshares-music" "jinn" "instantdex" "ico-openledger" "neucoin" "htmlcoin" "xcurrency" "asiacoin" "2give" "colossuscoin-v2" "btsr" "btctalkcoin" "librexcoin" "ultracoin" "leafcoin" "flycoin" "coinomat" "mediterraneancoin" "pandacoin-pnd" "liquid" "rare-pepe-party" "bigup" "kobocoin" "nxttycoin" "bitcointx" "sooncoin" "litedoge" "paycoin2" "sprouts" "the-cypherfunks" "tilecoin" "yacoin" "lottocoin" "wayguide" "swagbucks" "martexcoin" "putincoin" "quazarcoin" "checkcoin" "quotient" "trollcoin" "reecoin" "supercoin" "sproutsextreme" "globalcoin" "bitsilver" "gaia" "plncoin" "triangles" "ratecoin" "smartcoin" "bitbar" "ufo-coin" "arbit" "qubitcoin" "bitz" "nyancoin" "bitgold" "hamradiocoin" "viral" "datacoin" "satoshimadness" "mindcoin" "amsterdamcoin" "freicoin" "aricoin" "tigercoin" "cashout" "russiacoin" "petrodollar" "redcoin" "cypher" "emerald" "aurumcoin" "octocoin" "tittiecoin" "cagecoin" "crypto" "guncoin" "bunnycoin" "fastcoin" "beavercoin" "argentum" "philosopher-stones" "bumbacoin" "coexistcoin" "revolvercoin" "hundredcoin" "phoenixcoin" "biteur" "bitcoin-scrypt" "mgw" "cubits" "uniqredit" "halcyon" "bitzeny" "antibitcoin" "debitcoin" "flavorcoin" "vaperscoin" "kittehcoin" "leacoin" "ronpaulcoin" "prototanium" "dobbscoin" "captcoin" "spots" "icash" "mangocoinz" "dappster" "atomic-coin" "paycon" "cybercoin" "evotion" "wmcoin" "osmiumcoin" "breakcoin" "zurcoin" "ego" "metal-music-coin" "mmbtcd" "floz" "chesscoin" "pospro" "blazecoin" "vcoin" "bitcloud" "topcoin" "bitcoin-21" "popularcoin" "lanacoin" "guarany" "high-voltage" "acoin" "independent-money-system" "unicoin" "digitalprice" "b3coin" "jin-coin" "newyorkcoin" "elementrem" "parallelcoin" "orlycoin" "fuzzballs" "elcoin" "corgicoin" "nevacoin" "pulse" "machinecoin" "posex" "px" "helleniccoin" "c-bit" "mustangcoin" "cabbage" "unfed" "xonecoin" "comet" "eurocoin" "dpay" "tagrcoin" "virtualcoin" "bitquark" "revenu" "chaincoin" "selfiecoin" "cryptbit" "photon" "money" "cashcoin" "shilling" "bittokens" "bowscoin" "tajcoin" "ponzicoin" "stronghands" "antilitecoin" "litecred" "save-and-gain" "imperialcoin" "number7" "p7coin" "batcoin" "swaptoken" "crtcoin" "enigma" "californium" "digital-credits" "pizzacoin" "khancoin" "ernus" "23-skidoo" "satoshicard" "horiemoncard" "royalcoin-2" "nxttyacci" "forevercoin" "pluton" "e-dinar-coin" "bfx" "leocoin" "techshares" "clubcoin" "maskcoin" "wowecoin" "dynamiccoin" "asset-backed-coin" "heat-ledger" "the-dao" "edrcoin" "index-coin" "omicron" "mind-gene" "first-blood" "alpacoin" "npccoin" "gaycoin" "biglifecoin" "international-diamond" "uncoin" "tbcoin" "happy-creator-coin" "caliphcoin" "first-bitcoin" "invisiblecoin" "deltacredits" "gbcgoldcoin" "taopay" "timekoin" "futurepoints" "lecoin" "neptunecoin" "alphabet-coin-fund" "rhodiumcoin" "lomocoin" "bitland" "eclipse" "digitalfund" "enecoin" "kolschcoin" "sharkcoin" "bagcoin" "revcoin" "clinton" "president-johnson" "woodshares" "cthulhu-offerings" "gotfomo" "peacecoin" "cartercoin" "xaucoin" "shellpay" "advanced-internet-blocks" "royalcoin" "president-trump" "upcoin" "eggcoin" "futcoin" "rcoin" "goldmaxcoin" "cbd-crystals" "chncoin" "cleverbot" "bitalphacoin" "ocow" "trickycoin" "frankywillcoin" "psilocybin" "bitcoinfast" "rublebit" "kcoin" "local-family-owned" "richcoin" "digital-bullion-gold" "gameleaguecoin" "fedorashare" "pabyosicoin" "denarius" "flaxscript" "braincoin" "asiccoin" "avatarcoin" "xab" "motocoin" "todaycoin" "quebecoin" "cycling-coin" "linkedcoin" "dubstep" "operand" "opescoin" "mobilecash" "lazaruscoin" "sportscoin" "darklisk" "superstaketoken" "skeincoin" "president-clinton" "ugain" "lathaan" "fitcoin" "vegascoin" "sakuracoin" "paypeer" "moneta2" "digieuro" "prismchain" "pokechain" "teamup" "aces" "cashme" "bitmoon" "golfcoin" "nucleustokens" "valorbit" "superturbostake" "thecreed" "fireflycoin" "x2" "soulcoin" "safecoin" "pokecoin" "tellurion" "victoriouscoin" "ganjacoin-v2" "choofcoin" "xpay" "nutcoin" "paccoin"})

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

(s/fdef json-get
        :args (s/cat :url ::url :opts (s/? map?))
        :ret map?)

(defn json-get [url & opts]
      (as-> (cnt/get url (when opts (first opts))) x
            (:body x)
            (jsn/read-str x :key-fn keyword)))

(defn digit-count [no]
      (int (+ 1 (Math/floor (Math/log10 no)))))

(s/fdef to-human
        :args (s/cat :unix ::unixtimestamp))

(defn to-human [unix]
      (let [ts-length (digit-count unix)
            convert (fn [x] (.toString (c/from-long (long x))))]
           (cond
             (>= 10 ts-length) (convert (* 1000 unix))
             (< 10 ts-length) (convert unix)
             :else (convert unix))))

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


