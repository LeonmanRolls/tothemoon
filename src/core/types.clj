(ns core.types
    (:require
        [clojure.spec :as s]))

(s/def ::unixtimestamp
    (s/with-gen
        (s/and number? #(> 1577580878000 % 0))
        (fn [] (s/gen #{157758087000}))))




