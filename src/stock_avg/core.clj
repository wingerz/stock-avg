(ns stock-avg.core
  (:use [clojure.tools.cli :only [cli]])
  (:require [clj-http.client :as client])
  (:require [clojure-csv.core :as csv])
  (:require [clojure.math.numeric-tower :as math])
  (:gen-class))

(defn get-stock-data [ticker]
  (:body
  (client/get
   "http://ichart.finance.yahoo.com/table.csv?"
   {:query-params {"s" ticker "c" 2010}})))

(defn normalize-header [header]
  (->
   header
   clojure.string/lower-case
   (.replace " " "")
   keyword))

(defn parse-stock-data [raw-data]
  (let
      [csv-data (csv/parse-csv raw-data)
       header (first csv-data)
       rows (rest csv-data)
       ]
    (map
     (fn [row]
       (zipmap
        (map normalize-header header)
        row))
     rows)))

(defn moving-avg [data days]
  (let [data-in-range (take days data)]
    (/
     (reduce + (map
         (fn [x]
           (->
            x
            :close
            read-string
            ))
           data-in-range))
     (count data-in-range))))

(defn percent-diff [current comparison]
  (*
   (/
    (- current comparison)
    comparison
    )
   100))

(defn calculate-things [data]
  (let
      [recent (-> data first :close read-string)
       previous (-> data second :close read-string)
       recent-change (- recent previous)
       avg-200 (moving-avg data 200)
       avg-300 (moving-avg data 300)]
    {
     :recent recent
     :recent-change recent-change
     :moving-avg-200 avg-200
     :diff-200 (/ recent-change avg-200)
     :moving-avg-300 avg-300
     :diff-300 (/ recent-change avg-300)
     :count (count data)
     }))

(defn format-percent [val]
  (format "%.2f" (* val 100)))

(defn format-price [val]
  (format "%.2f" val))

(defn is-special-condition [calculated-data]
  (or
   (> (math/abs (:diff-200 calculated-data)) 0.01)
   (> (math/abs (:diff-300 calculated-data)) 0.01)))

(defn output-header []
  (clojure.string/join
   "\t"
   [
    "sym"
    "recent"
    "change"
    "200d"
    "% vs"
    "300d"
    "% vs"
    "of note"
    "days"
    ])

  )
(defn output-ticker [ticker calculated-data]
  (clojure.string/join
   "\t"
   [
    ticker
    (format-price (:recent calculated-data))
    (format-price (:recent-change calculated-data))
    (format-price (:moving-avg-200 calculated-data))
    (format-percent (:diff-200 calculated-data))
    (format-price (:moving-avg-300 calculated-data))
    (format-percent (:diff-300 calculated-data))
    (cond
     (is-special-condition calculated-data) "*"
     :else "")
    (cond
     (< (:count calculated-data) 200) "lt200"
     (< (:count calculated-data) 300) "lt300"
     :else "")
    ]))

(defn parse-args [args]
  (first
   (cli args
        ["-t" "--ticker" "ticker symbol"
         :assoc-fn (fn [previous key val]
                     (assoc previous key
                            (if-let [oldval (get previous key)]
                              (merge oldval val)
                              (hash-set val))))])))

(defn calculate-for-ticker [ticker]
  (-> ticker
      get-stock-data
      parse-stock-data
      calculate-things))

(defn -main [& args]
  (let
      [tickers (:ticker (parse-args args))
       ticker-data (zipmap
                    (map symbol tickers) 
                    (map calculate-for-ticker tickers))]
    (println
     (clojure.string/join
      "\n"
      (cons
       (output-header)
       (map (fn [ticker]
             (output-ticker ticker ((symbol ticker) ticker-data))
             ) 
           tickers))))))