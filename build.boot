(set-env!
  :dependencies
  '[[org.clojure/clojure "1.9.0-alpha10"]
    [org.clojure/test.check "0.9.0" :scope "test"]
    [org.clojure/core.logic "0.8.11"]
    [clj-http "2.2.0"]
    [org.clojure/data.json "0.2.6"]
    [clojurewerkz/envision "0.1.0-SNAPSHOT"]
    [incanter "1.5.7"]
    [clj-time "0.12.0"]]
  :source-paths #{"src"})

(require
  '[clojure.spec :as s]
  '[clojure.spec.gen :as gen]
  '[clojure.spec.test :as ts :refer [check]]
  '[clojure.spec.gen :as gen]
  '[clojure.tools.namespace.repl :as repl]
  '[core.utils :as u]
  '[core.spike :as sp]
  '[core.simple :as smp])

(comment

 (smp/simple-strat (:Data (u/json-get (u/cryptocompare-url-gen "BTC" "USD" "histoday" 1000))))

  (do
    (load-file "src/core/utils.clj")
    (load-file "src/core/simple.clj")
    (load-file "src/core/spike.clj")
    (ts/unstrument)
    (ts/instrument))

  (ts/summarize-results
    (ts/check
      'core.utils/json-get
      {:clojure.spec.test.check/opts {:num-tests 1}}))

  (ts/summarize-results
    (ts/check
      (ts/checkable-syms)
      {:clojure.spec.test.check/opts {:num-tests 1}}))
  )


