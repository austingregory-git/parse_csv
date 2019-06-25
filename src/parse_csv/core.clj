(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
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