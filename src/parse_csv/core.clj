(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.walk :as walk]
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

(defn remove-white-space
  [row]
  (into [] (remove (fn [z]
                     (= (count z) 0)) row)))

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
; (def k (partition 2 (get j 1)))
; (into {} (map vec k))
; => {"NLC Address" "1234 Johnson Dr.", "Invoice Date" "06/25/2019"}
; (clojure.walk/keywordize-keys (into {} (map vec k)))
; => {:NLC Address, "1234 Johnson Dr." :Invoice, Date "06/25/2019"}
;(clojure.string/replace "NLC Address" #" " "-")
;=> "NLC-Address"
; (def h (into {} (map vec k)))
; (def g (into [] (map vec k)))


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