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
  '[clojure.core.reducers :as rd]
  '[core.utils :as u]
  '[core.spike :as sp]
  '[core.types :as tps]
  '[core.simple :as smp])

(deftask my-task
         "Does nothing."
         []
         (fn [next-task]
             (fn [fileset]
                 (println "hi hi")
                 (next-task fileset))))

(comment

  (def raw-data (u/json-get (u/cryptocompare-url-gen "BTC" "USD" "histoday" 1000)))
  (u/to-human (first (:Data raw-data)))

  (def rslt (smp/simple-strat
              (:Data raw-data)
              :human))

  (def reds (:reds rslt))

  (do
    (load-file "src/core/types.clj")
    (load-file "src/core/utils.clj")
    (load-file "src/core/simple.clj")
    (load-file "src/core/spike.clj")
    (load-file "build.boot")
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


