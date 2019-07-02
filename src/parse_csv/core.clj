(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [dk.ative.docjure.spreadsheet :as spreadsheet]))

;; [pdfboxing.text :as text]

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn read-csv
  [filename]
  (into [] (with-open [reader (io/reader filename)]
     (doall
       (csv/read-csv reader)))))

;;(def x (read-csv "example2.csv"))
;; (.indexOf (nth x 1) "NLC Name") => 1
;(nth x 1)
;=> ["" "NLC Name" "Soup, Inc" "" "" "" "" "" "Invoice No" "123456"]
;; (get (nth x 1) (.indexOf (nth x 1) "NLC Name"))
;=> "NLC Name"
;(get (nth x 1) (+ (.indexOf (nth x 1) "NLC Name") 1))
;=> "Soup, Inc"
;;(get (nth x 1) (inc (.indexOf (nth x 1) "NLC Name")))
;=> "Soup, Inc"

(defn reduce-csv-row
  "Accepts a csv-row (a vector) a list of columns to extract,
   and reduces (and returns) a csv-row to a subset based on
   selection using the values in col-nums (a vector of integer
   vector positions.)"

  [csv-row col-nums]

  (reduce
    (fn [out-csv-row col-num]
      ; Don't consider short vectors containing junk.
      (if-not (<= (count csv-row) 1)
        (conj out-csv-row (nth csv-row col-num nil))))
    []
    col-nums))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn remove-white-space
  [row]
  (into [] (remove (fn [z]
                     (= (count z) 0)) row)))

(defn replace-bad-with-dash
  [string]
  (clojure.string/replace (clojure.string/replace string #":" "") #"[/\ +]" "-"))

(defn keywordize
  [m]
  (let [f (fn [[key val]]
            (if (string? key)
              [(keyword (string/lower-case (replace-bad-with-dash key))) val]
              [key val]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x))
              m)))

(defn map-keys
  [f m]
  (persistent!
    (reduce-kv (fn [m key val] (assoc! m (f key) val))
               (transient (empty m)) m)))

(defn macro-parse-csv-to-if
  [filename]
  (->> filename
      (read-csv ,,,)
      (map remove-white-space ,,,)
       (remove empty? ,,,)
       (take 3 ,,,)
       (into [] ,,,)
       (get ,,, 1)))

(defn row-to-map
  [row]
  (let [paired-row (partition 2 row)
        vectorized-pairs (map vec paired-row)
        mapped-pairs (into {} vectorized-pairs)
        keywordized-mapped-pairs (keywordize mapped-pairs)]
    keywordized-mapped-pairs))

(defn parse-csv-to-if
  [filename]
  (let [contents (read-csv filename)
        ws-removed (map remove-white-space contents)
        empty-removed (remove empty? ws-removed)
        vectorized (into [] empty-removed)
        vector-of-rows (mapv row-to-map vectorized)
        useful-rows (take-while (fn [z] (> (count z) 0)) vector-of-rows)
        final-form (into {} (into [] useful-rows))]
    final-form))

;; validate nlc invoices

(defn validate-date
  [coll]
  (let [given-date (get coll :invoice-date)
        split-date (string/split given-date #"/")
        month (get split-date 0)
        day (get split-date 1)
        year (get split-date 2)
        validated-month (<= (read-string month) 12)
        validated-day (<= (read-string day) 31)
        validated-year (or (= (count year) 2) (= (count year) 4))
        validated-date (and validated-month validated-day validated-year)]
    validated-date))

(defn validate-invoice-no
  [coll]
  (let [given-inv-no (get coll :invoice-no)
        validated-char-lim (<= (count given-inv-no) 12)
        validated-no-space-test (not (string/includes? given-inv-no " "))
        validated-invoice-no (and validated-char-lim validated-no-space-test)]
    validated-invoice-no))

(defn validate-billing-dept
  [coll]
  (let [given-bd (get coll :billing-department)
        validated-bd (in? ["Taxonomy"
                           "Engineering"
                           "Product"
                           "Marketing"
                           "Client Engagement"
                           "BD"
                           "Finance"
                           "People & Culture"
                           "Corp IT"
                           "Corporate"
                           "R&D"] given-bd)]
    validated-bd))

(defn validate-taxonomy
  [coll]
  (let [given-taxonomy (get coll :taxonomy-projects)
        validated-taxonomy (in? ["Special_Projects"
                                 "Danish_Denmark"
                                 "Dutch_Netherlands"
                                 "English_Australia"
                                 "English UK"
                                 "English_US"
                                 "English_US_Spanish_Mexico"
                                 "Finnish_Finland"
                                 "Flemish_Belgium"
                                 "French_Canada"
                                 "French_Belgium"
                                 "French_France"
                                 "German_Germany"
                                 "IOL_Italy (fka seat)"
                                 "Italian_Italy"
                                 "Norwegian_Norway"
                                 "Paginas_Budg_Portuguese_Portugal"
                                 "Portuguese_Brazil"
                                 "Spanish_Argentina"
                                 "Spanish_Colombia"
                                 "Spanish_Latin America"
                                 "Spanish_Mexico"
                                 "Spanish_Spain"
                                 "Swedish_Sweden"] given-taxonomy)]
    validated-taxonomy))

(defn validate-nlc-name
  [coll]
  (let [given-name (get coll :nlc-name)
        validated-nlc-name (> (count given-name) 0)]
    validated-nlc-name))

(defn validate-nlc-address
  [coll]
  (let [given-address (get coll :nlc-address)
        validated-nlc-address (> (count given-address) 0)]
    validated-nlc-address))

(defn validate-nlc-phone
  [coll]
  (let [given-phone (get coll :nlc-phone)
        phone-number (string/replace given-phone #"-" "")
        validated-nlc-phone (= (count phone-number) 10)]
    validated-nlc-phone))

(defn validate-nlc-invoice
  [coll]
  (println "Billing:" (validate-billing-dept coll))
  (println "Taxonomy:" (validate-taxonomy coll))
  (println "Name:" (validate-nlc-name coll))
  (println "Address:" (validate-nlc-address coll))
  (println "Phone Number:" (validate-nlc-phone coll))
  (println "Date:" (validate-date coll))
  (println "Invoice No:" (validate-invoice-no coll))
  (and (validate-billing-dept coll)
       (validate-taxonomy coll)
       (validate-nlc-name coll)
       (validate-nlc-address coll)
       (validate-nlc-phone coll)
       (validate-date coll)
       (validate-invoice-no coll)))

;;validate contractor invoices

(defn validate-contr-name
  [coll]
  (let [given-name (get coll :name)
        validated-contr-name (> (count given-name) 0)]
    validated-contr-name))

(defn validate-contr-address
  [coll]
  (let [given-address (get coll :address)
        validated-contr-address (> (count given-address) 0)]
    validated-contr-address))

(defn validate-contr-phone
  [coll]
  (let [given-phone (get coll :phone)
        phone-number (string/replace given-phone #"-" "")
        validated-contr-phone (= (count phone-number) 10)]
    validated-contr-phone))

(defn validate-contr-invoice
  [coll]
  (println "Billing:" (validate-billing-dept coll))
  (println "Name:" (validate-contr-name coll))
  (println "Address:" (validate-contr-address coll))
  (println "Phone Number:" (validate-contr-phone coll))
  (println "Date:" (validate-date coll))
  (println "Invoice No:" (validate-invoice-no coll))
  (and (validate-billing-dept coll)
       (validate-contr-name coll)
       (validate-contr-address coll)
       (validate-contr-phone coll)
       (validate-date coll)
       (validate-invoice-no coll)))

;;(def x (read-csv "example2.csv"))
;;(def y (nth x 1))
;;(reduce-csv-row y [1 2 8 9])
;; => ["NLC Name" "Soup, Inc" "Invoice No" "123456"]
;;(remove (fn [x]
;;          (= (count x) 0)) y)
;; => ("NLC Name" "Soup, Inc" "Invoice No" "123456")
;;(filter (fn [x] (not= (count x) 0)) y)
;=> ("NLC Name" "Soup, Inc" "Invoice No" "123456")
;                   (= (count z) 0)) y))
;  => ["NLC Name" "Soup, Inc" "Invoice No" "123456"]
; (remove empty? (map remove-white-space x))
; (take 3 (remove empty? (map remove-white-space x)))
; (def j (into [] (take 3 (remove empty? (map remove-white-space x)))))
; (partition 2 (get j 1))
;=> (("NLC Address" "1234 Johnson Dr.") ("Invoice Date" "06/25/2019"))
; (partition 2 (get j 2))
;=> (("NLC Phone" "222-333-4444"))
; (def k (partition 2 (get j 1)))
; (into {} (map vec k))
; => {"NLC Address" "1234 Johnson Dr.", "Invoice Date" "06/25/2019"}
; (clojure.walk/keywordize-keys (into {} (map vec k)))
; => {:NLC Address, "1234 Johnson Dr." :Invoice, Date "06/25/2019"}
;(clojure.string/replace "NLC Address" #" " "-")
;=> "NLC-Address"
; (def h (into {} (map vec k)))
; (def g (into [] (map vec k)))
; (keywordize h)
;=> {:NLC-Address "1234 Johnson Dr.", :Invoice-Date "06/25/2019"}
; (row-to-map (get j 1))
;=> {:NLC-Address "1234 Johnson Dr.", :Invoice-Date "06/25/2019"}
;; (def billing ["Taxonomy" "Engineering" "Product" "Marketing" "Client Engagement" "BD" "Finance" "People & Culture" "Corp IT" "Corporate" "R&D"])

(defn find-headers [filename])

(defn csv-data->maps [csv-data]
   (into [] (map hash-map
                 (->> (first csv-data) ;; First row is the header
                      (map keyword) ;; Drop if you want string keys instead
                      repeat)
                 (rest csv-data))))

(csv-data->maps (read-csv reader))

(defn read-rows [filename]
  (->>
    (spreadsheet/load-workbook filename)
    (spreadsheet/select-sheet "Sheet1")
    (spreadsheet/select-columns {:A :a-name :B :b-name :C :c-name :D :d-name :E :e-name :F :f-name :G :g-name :H :h-name :I :i-name})))

(defn read-pdf [filename]
  (text/extract filename))