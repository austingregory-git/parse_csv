(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn read-csv
  [filename]
  (into [] (with-open [reader (io/reader filename)]
     (doall
       (csv/read-csv reader)))))