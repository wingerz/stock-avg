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

(defn year-and-month [date]
  (let [date-ymd (map (fn [x] (. Integer parseInt x)) (clojure.string/split date #"-"))]
    (vector (first date-ymd) (second date-ymd))))

(defn month-ends [data]
  (let [group-data (group-by 
                    (fn [data-for-day]
                      (year-and-month (:date data-for-day)))
                    data)
        months (reverse (sort (keys group-data)))
        ]
    (rest ; don't include the current month
     (map
      (fn [x]
        (first (group-data x)))
      months))
    ))

(defn calculate-things [data]
  (let
      [recent (-> data first :close read-string)
       previous (-> data second :close read-string)
       recent-change (- recent previous)
       avg-200 (moving-avg data 200)
       month-ends (month-ends data)
       recent-month-end (-> month-ends first :close read-string)
       month-ends-10 (take 10 month-ends)
       month-ends-avg-10 (/ (reduce +
                                       (map
                                        (fn [x]
                                          (read-string (:close x)))
                                        month-ends-10))
                               (count month-ends-10))
       ]
    {
     :recent recent
     :recent-change recent-change
     :moving-avg-200 avg-200
     :diff-200 (/ recent-change avg-200)
     :month-end recent-month-end
     :month-ends-avg-10 month-ends-avg-10
     :count (count data)
     }))

(defn format-percent [val]
  (format "%.2f" (* val 100)))

(defn format-price [val]
  (format "%.2f" val))

(defn is-special-condition [calculated-data]
  (or
   (> (math/abs (:diff-200 calculated-data)) 0.01)
   (< (:month-end calculated-data) (:month-ends-avg-10 calculated-data))))

(defn output-header []
  (clojure.string/join
   "\t"
   [
    "sym"
    "month"
    "10month"
    "recent"
    "change"
    "200d"
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
    (format-price (:month-end calculated-data))
    (format-price (:month-ends-avg-10 calculated-data))
    (format-price (:recent calculated-data))
    (format-price (:recent-change calculated-data))
    (format-price (:moving-avg-200 calculated-data))
    (format-percent (:diff-200 calculated-data))
    (cond
     (is-special-condition calculated-data) "*"
     :else "")
    (cond
     (< (:count calculated-data) 200) "lt200"
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