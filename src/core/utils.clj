(ns core.utils
    (:require
      [clj-http.client :as cnt]
      [clojure.data.json :as jsn]
      [core.types :as tps]
      [clojure.spec :as s]
      [clojure.spec.gen :as gen]
      [clojure.spec.test :as ts :refer [check]]
      [clojure.spec.gen :as gen]
      [clj-time.coerce :as c]))

(def cryptocompare-syms #{"MCN" "FRN" "SDP" "DGORE" "HYP" "PAK" "2BACCO" "CPC" "NEOS" "GRN" "TWLV" "MND" "RIPO" "VTA" "HAL" "RCX" "ENRG" "1337" "NUBIS" "LANA" "BEN" "2015" "ZRC" "IOC" "CJ" "CDX" "BST" "SQL" "VPRC" "NRS" "BTCRY" "STEPS" "CNL" "LQD" "DT" "ADZ" "HALLO" "1CR" "DASH" "EQUAL" "MST" "BYC" "BSTY" "XT" "NTC" "XHI" "SOLE" "MINT" "TRIG" "CRW" "LAB" "BAY" "RISE" "RZR" "GEN" "CACH" "CLINT" "WBB" "UIS" "BTCL" "SNRG" "REP" "MOIN" "QTL" "ISL" "XBOT" "ANS" "NEU" "SC" "MOOND" "CYP" "START" "OK" "GP" "BCR" "NRB" "MC" "EDRC" "GAY" "BOTS" "611" "EQM" "VRC" "GRS" "SH" "CRBIT" "KEY" "MIL" "ARG" "RT2" "OC" "GOON" "LXC" "MSC" "GB" "DGD" "ROYAL" "BLC" "LSD" "GAP" "CRX" "FTP" "1ST" "RDD" "ADCN" "LTB" "CHILD" "NXTI" "TAGR" "NEBU" "XPM" "MUE" "AERO" "CRE" "NYAN" "FIRE" "MRP" "UTIL" "NZC" "COC" "UNC" "NAUT" "CRACK" "CMC" "DIME" "CC" "CHOOF" "BLK" "FCT" "TEAM" "FLT" "SBD" "SWEET" "WOLF" "BM" "MAPC" "HZ" "PYC" "ZOOM" "ALF" "42" "XNX" "TELL" "ERC" "TRON" "SWIFT" "NOTE" "DKC" "ANTI" "CKC" "SRC" "AMP" "007" "GIVE" "EMIGR" "TRUST" "MYR" "EKN" "EXCL" "CLR" "CALC" "YOC" "PSEUD" "GRT" "GPU" "DOT" "UNB" "C2" "CBX" "QTZ" "LYC" "XRE" "CTC" "DTC" "ERB" "CHASH" "AMC" "BOB" "SKB" "LC" "LEA" "DB" "NKC" "MI" "XCR" "XMG" "LYB" "ELE" "CRC" "AEC" "WC" "LTC" "QRK" "PKB" "XWT" "DCNT" "TOR" "CHA" "FRC" "CURE" "CIN" "LDOGE" "NSR" "SYNC" "EAGS" "TRI" "SHLD" "SAK" "NANAS" "BNCR" "GLX" "CAM" "VTR" "GMC" "CYT" "SFE" "CARBON" "ZED" "SBC" "AIB" "DEUR" "SCT" "CIRC" "BCN" "NMC" "BITB" "XTC" "TMC" "STO" "QBK" "BCX" "VRM" "AEON" "CSC" "KMD" "VTC" "ALN" "NAV" "LBC" "PEERPLAYS" "ZNY" "JBS" "ROS" "BTC" "VIA" "BURST" "MG" "AND" "XAI" "DLISK" "NKT" "CLUB" "LTH" "UNAT" "DBG" "IBANK" "BYTES" "GSY" "EDC" "EXP" "SPC" "KGC" "XDN" "XUP" "BLU" "ETH" "BTG" "GROW" "GCC" "CINNI" "MMC" "AIR" "EC" "VIP" "STR*" "TEK" "XXX" "GCR" "DGDC" "SLG" "KOBO" "SAT2" "LGBTQ" "UNITY" "BET" "UNIQ" "XCE" "MOON" "FFC" "ASN" "XID" "HODL" "GLOBE" "MAX" "GRC" "SLS" "OCTO" "MNM" "JUDGE" "BTE" "SCRPT" "TRK" "FSC2" "RMS" "VLT" "XPH" "TDFB" "STEEM" "DEM" "PSY" "RUST" "MEME" "CON" "JKC" "BLOCKPAY" "HKG" "ELC" "PXL" "ZCC" "ORB" "RCN" "STHR" "BAC" "XC" "OBITS" "SWING" "DGC" "THC" "FLAP" "MONETA" "XPY" "NTRN" "VAPOR" "APC" "ORLY" "MONA" "NUM" "DSB" "J" "WGC" "JOBS" "CLV" "OPAL" "SPOTS" "BOOM" "ARCH" "MED" "BQC" "TES" "EAC" "KUBO" "SXC" "SSD" "HTC" "PWR" "PINK" "CUBE" "CRYPT" "XVG" "BUK" "VPN" "EVENT" "INV" "SMLY" "PDC" "ACOIN" "INCP" "CRAFT" "GSM" "FOREX" "SEN" "PBC" "BTX" "EMC2" "HILL" "HVC" "UTH" "LSK" "BUCKS" "SUPER" "VTX" "KRAK" "STS" "RYC" "FC2" "LEMON" "DIGS" "XBS" "TX" "SPRTS" "OMNI" "NEC" "MPRO" "BBCC" "OMC" "RPC" "XWC" "BLOCK" "SLING" "ARDR" "XMS" "CF" "GRID" "LFC" "ANC" "CFC" "N7" "COOL" "TCR" "PIZZA" "SDC" "PRE" "TTC" "PRIME" "KARM" "VERSA" "ETC" "DGB" "NAN" "XRP" "X2" "BRK" "TOT" "DISK" "WDC" "TRA" "SMAC" "GDC" "MUDRA" "NBT" "MDC" "JIF" "FLY" "FCN" "EXB" "CHESS" "DVC" "MARV" "BTMI" "2FLAV" "PRM" "DNET" "CLOAK" "PTS" "MIN" "EGO" "BERN" "XRA" "INFX" "REE" "ULTC" "BELA" "SOUL" "LIMX" "VOOT" "GAIA" "XG" "DCR" "HNC" "FX" "JPC" "CAP" "LUX" "VEC2" "SMSR" "GIG" "RING" "GRM" "DRZ" "XAU" "M1" "GBT" "RBIT" "SCRT" "GIZ" "TODAY" "KAT" "TAG" "ZET2" "DROP" "COX" "SPHR" "XAUR" "ZYD" "RBT" "FCS" "BRONZ" "SCN" "SCOT" "QSLV" "B3" "POLY" "INCH" "OMA" "ANTC" "AC" "MMNXT" "SPM" "TUR" "MRS" "COV" "SIGU" "HBN" "TIA" "GHC" "WAY" "CHIP" "DMD" "DES" "FRK" "DIEM" "TAM" "SUP" "PXC" "DRKT" "CANN" "UNIT" "XZC" "CNMT" "CYG" "CTO" "QORA" "GLYPH" "SPX" "GLD" "OLYMP" "BTD" "XMR" "2GIVE" "XDE2" "CAT" "PRC" "LAZ" "TRUMP" "FONZ" "CRNK" "DCC" "KTK" "ENE" "COVAL" "UFO" "RRT" "GRAV" "SMC" "DLC" "FLO" "EGG" "CXC" "ION" "LK7" "GMX" "HMP" "KRB" "PHS" "BTA" "EXIT" "SPORT" "HVCO" "STRAT" "LOG" "LTCD" "RDN" "TKN" "VIOR" "UBIQ" "CSH" "REV" "RIC" "EGC" "BIOS" "HIRE" "BITUSD" "GML" "ICN" "BITCNY" "FRAC" "ACP" "FAIR" "CYC" "MN" "DOGE" "MZC" "NOBL" "AUR" "CASH" "FIBRE" "MARS" "OSC" "FRWC" "BOST" "BUZZ" "EVIL" "CRAIG" "KORE" "SOON" "CESC" "LTCX" "DANK" "KRC" "LKY" "AGS" "GREXIT" "GHS" "XDB" "GLC" "SHF" "GOTX" "ICB" "VIRAL" "NEVA" "PSI" "MAID" "BITS" "PSB" "CLUD" "RBR" "CGA" "VOX" "SLM" "GNJ" "XDP" "MMXIV" "BON" "NBL" "BITZ" "XLB" "GPL" "404" "XFC" "STR" "EZC" "NAS2" "TIT" "FJC" "WINGS" "CLAM" "BTCR" "ZNE" "XQN" "CRAB" "DCK" "BTM" "USDE" "COIN" "XST" "XNA" "MYST" "DSH" "NXS" "XCP" "SNS" "GEO" "DBTC" "SLR" "NVC" "CNC" "TENNET" "ARM" "TRC" "RBY" "XBC" "PIO" "I0C" "ZUR" "BCY" "NKA" "SPACE" "AXR" "32BIT" "FLDC" "HYPER" "BIGUP" "GUE" "EMD" "SANDG" "SPR" "MNE" "POST" "IXC" "NXTTY" "BHC" "WEX" "WAVES" "EDGE" "BTS" "ZEIT" "BSC" "GAME" "NET" "SYS" "IVZ" "KNC" "CCN" "ATM" "SONG" "TAB" "UNO" "STA" "STV" "MWC" "LTBC" "ZET" "HUGE" "DCS." "VMC" "MT" "XCO" "CAIX" "YBC" "RADS" "XEM" "BTQ" "BXT" "LGD" "HEAT" "OLDSF" "COMM" "NLC" "WMC" "GHOUL" "ROOT" "EXE" "METAL" "NODE" "BEATS" "CRAVE" "MRY" "CREVA" "SSTC" "FLX" "XPD" "MOJO" "MARYJ" "MEC" "ARI" "ETHS" "AMS" "VOYA" "SJCX" "SILK" "LIR" "SEC" "CV2" "SYNX" "SIB" "SOIL" "SAR" "DBIC" "TGC" "GBRC" "BLITZ" "DOPE" "BTB" "BLRY" "DOGED" "LYKKE" "POINTS" "U" "CAB" "XDQ" "SPEC" "RBIES" "GAM" "MTR" "ICASH" "SPA" "UTC" "GEMZ" "XVC" "XSI" "FTC" "EMC" "AXIOM" "LTS" "HEDG" "BS" "ENTER" "BBR" "VTY" "ADC" "DPAY" "PEC" "FUZZ" "KR" "UNF" "PUTIN" "BIT16" "POT" "SNGLS" "EPY" "PNK" "CMT" "NMB" "NXT" "FSN" "GSX" "808" "IFC" "OBS" "BFX" "BTCD" "BRAIN" "DUB" "BNT" "VDO" "YAC" "PULSE" "ABY" "ZEC" "BOLI" "MYC" "ADN" "SPT" "YOVI" "RUBIT" "EBS" "HCC" "ARB" "PTC" "ATOM" "FUTC" "QCN" "NXE" "SWARM" "CETI" "NETC" "ANNC" "888" "DRKC" "DRACO" "GRAM" "APEX" "AM" "KDC" "SFR" "XPOKE" "STAR" "HUC" "8BIT" "CELL" "EFL" "TAK" "FST" "SHREK" "SHADE" "EKO" "XCN" "FIT" "PXI" "SSV" "PINKX" "MINE" "XPB" "PIGGY" "URO" "XSEED" "NLG" "JWL" "XPO" "MNC" "PPC" "WARP" "BSD" "XCASH" "XJO" "AMBER"})

(s/def ::cryptocomapre-sym cryptocompare-syms)

(s/def ::fiat-sym #{"USD" "GBP" "EUR"})

(s/def ::timescale #{"histominute" "histohour" "histoday"})

(s/def ::url (s/with-gen
               (s/and string? #(.contains % "://"))
               (fn [] (s/gen #{"https://www.cryptocompare.com/api/data/coinlist/"}))))

(s/fdef cryptocompare-url-gen
        :args (s/cat :fsym ::cryptocomapre-sym :tsym ::fiat-sym
                     :timescale ::timescale :limit #(> 1001 %  0))
        :ret ::url)

(defn cryptocompare-url-gen [fsym tsym timescale limit]
      (str
        "https://www.cryptocompare.com/api/data/"
        timescale
        "/?e=CCCAGG"
        "&fsym=" fsym
        "&tsym=" tsym
        "&limit=" limit))

(s/fdef json-get
        :args (s/cat :url ::url)
        :ret map?)

(defn json-get [url & header-map]
      (as-> (cnt/get url (when header-map {:headers (first header-map)})) x
            (:body x)
            (jsn/read-str x :key-fn keyword)))

(s/fdef to-human
        :args (s/cat :unix :tps/unixtimestamp))

(defn to-human [unix]
      (.toString (c/from-long (* 1000 (long unix)))))

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

