; Author: Austin Gregory with supervision/assistance from Dorab
; Software Consultant Intern Project --
; Automation of invoice/bill validation and IIF construction for importing into quickbooks for MatchCraft
; How to use:
; The master function used for bills (NLC/Contractor invoices--quickbooks considers these bills) is pdf-dir-to-iif-filename
; By calling this function and giving it a directory name which contains bills (in PDF format), and an output filename,
; The function will parse each file in the directory and convert it to an internal form, perform validation checks, will inform the user
; on the validation process, and if reading and validation succeeds, it will write to the given output file in IIF format
; so that it can be imported to quickbooks.
; To import into all bills into quickbooks, all you need is the iif file.
; In quickbooks, go to (File->Utilities->Import->IIF Files...) and select the iif file to import.
; Duplicate transactions are allowed into quickbooks, so be warned! You can use the old import method (at the bottom of the IIF file import window labeled import without review -- it has more reviewing than the other one...),
; which is better at warning you about duplication, but upon import fails to use the associated account to populate useful fields (Terms, Addresses, etc).
; Bills will populate
; ---
; The master function used for invoices (client invoices) is invoice-dir-to-iif-file
; By calling this function and giving it a directory name which contains invoices (in CSV format), and an output filename,
; The function will parse each file in the directory and convert it to an internal form, perform validation checks, will inform the user
; on the validation process, and if reading and validation succeeds, it will write to the given output file in IIF format
; Importation to quickbooks is the same as above.



(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [pdfboxing.text :as text]
            [clojure.edn :as edn]))

;;"variables"

(def billing-department-to-quickbooks-account
  (hash-map :bd "Outside Services:General Consulting"
            :r&d "Outside Services:R&D"
            :engineering "Outside Services:Engineering"
            :product "Outside Services:Engineering"
            :marketing "Outside Services:Marketing"
            :client-engagement "Outside Services:Client Services"
            :finance "Outside Services:General Consulting"
            :people&culture "Outside Services:General Consulting"
            :corp-it "Outside Services:General Consulting"
            :corporate "Outside Services:General Consulting"
            :taxonomy-projects {:special-projects "Outside Services:Taxonomy-Other"
                                :danish_denmark "NLCs:Taxonomy Development:Non-English (dev):Danish_Denmark:"
                                :dutch_netherlands "NLCs:Taxonomy Development:Non-English (dev):Dutch_Netherlands:"
                                :english_australia "NLCs:Taxonomy Development:English (dev):English_Australia:"
                                :english_uk "NLCs:Taxonomy Development:English (dev):English_UK:"
                                :english_us "NLCs:Taxonomy Development:English (dev):English_US:"
                                :english_us_spanish_mexico "NLCs:Taxonomy Development:English (dev):English_US_Spanish_Mexico:"
                                :finnish_finland "NLCs:Taxonomy Development:Non-English (dev):Finnish_Finland:"
                                :flemish_belgium "NLCs:Taxonomy Development:Non-English (dev):Flemish_Belgium:"
                                :french_canada "NLCs:Taxonomy Development:Non-English (dev):French_Canada:"
                                :french_belgium "NLCs:Taxonomy Development:Non-English (dev):French_Belgium:"
                                :french_france "NLCs:Taxonomy Development:Non-English (dev):French_France:"
                                :german_germany "NLCs:Taxonomy Development:Non-English (dev):German_Germany:"
                                :iol_italy "NLCs:Taxonomy Development:Non-English (dev):IOL_Italy (fka seat):"
                                :italian_italy "NLCs:Taxonomy Development:Non-English (dev):Italian_Italy:"
                                :norwegian_norway "NLCs:Taxonomy Development:Non-English (dev):Norwegian_Norway:"
                                :paginas_budg_portuguese_portugal "NLCs:Taxonomy Development:Non-English (dev):Paginas_Budg_Portuguese_Portugal:"
                                :portuguese_brazil "NLCs:Taxonomy Development:Non-English (dev):Portuguese_Brazil:"
                                :spanish_argentina "NLCs:Taxonomy Development:Non-English (dev):Spanish_Argentina:"
                                :spanish_colombia "NLCs:Taxonomy Development:Non-English (dev):Spanish_Colombia:"
                                :spanish_latin_america "NLCs:Taxonomy Development:Non-English (dev):Spanish_Latin_America:"
                                :spanish_mexico "NLCs:Taxonomy Development:Non-English (dev):Spanish_Mexico:"
                                :spanish_spain "NLCs:Taxonomy Development:Non-English (dev):Spanish_Spain:"
                                :swedish_sweden "NLCs:Taxonomy Development:Non-English (dev):Swedish_Sweden:"}))

;;this is automatically done in code using client-name, but if the "manual" hash-map is needed, here it is!
(def client-name-to-accounts-receivable
  {:liturgicalpub "Accounts Receivable-USD:liturgicalpub-A/R"
   :zipch "Accounts Receivable-CHF:zipch-A/R"})

(def required-nlc-fields
  ["NLC Name" "NLC Address" "NLC Phone" "Taxonomy/Projects" "Billing Department" "Amount Total"])

(def required-contractor-fields
  ["Name" "Address" "Phone" "Billing Department" "Total Due"])

(def optional-nlc-fields
  [])

(def optional-contractor-fields
  [])

(def nlc-invoice-fields
  (into [] (concat required-nlc-fields optional-nlc-fields)))

(def contractor-invoice-fields
  (into [] (concat required-contractor-fields optional-contractor-fields)))

;; Base hash-map for client-name as a key which gives the client-code to be used to generate an invoice number
(def client-name-to-client-code
  (hash-map :liturgicalpub "litpub"
            :zipch "zipch"))

;; Base hash-maps used to manually populate data.txt, but this is really only used for testing, as data.txt can be populated automatically
(def client-name-to-last-invoice-number
  (hash-map :liturgicalpub-last-invoice-number "FIRSTENTRY1"
            :zipch-last-invoice-number "FIRSTENTRY1"))

(def nlc-name-to-last-invoice-number
  (hash-map :mira-scott-last-invoice-number "FIRSTENTRY1"
            :eleonora-mazzanti-last-invoice-number "FIRSTENTRY1"))

(def contractor-name-to-last-invoice-number
  {})

(def client-name-list
  ["liturgicalpub" "zipch"])

(def nlc-name-list
  ["Mira Scott" "Eleonora Mazzanti"])

(def transaction-types
  ["BILL" "INVOICE"])


;;parsing functions

(defn read-csv
  "Given a csv filename, read CSV file's contents into a vector of strings"
  [filename]
  (into [] (with-open [reader (io/reader filename)]
             (doall
               (csv/read-csv reader)))))

(defn read-pdf
  "Given a pdf filename, read PDF's text contents into a string"
  [filename]
  (text/extract filename))

;;helper functions

(defn list-filenames
  "Given a filepath, list all file names of the given directory"
  [path]
  (seq (.list (clojure.java.io/file path))))

(defn in?
  "Given a collection and an element, give true if the collection contains that element"
  [coll elm]
  (some #(= elm %) coll))

(defn validize-total-due
  "Given a value which represents a total amount, convert it to a format that can be validated"
  [value]
  (if (string/includes? value ",")
    (if (< (count (subs value (inc (string/last-index-of value ",")))) 3)
      (string/replace (string/replace value #"\$" "") #"," ".")
      (string/replace value #"\$" ""))
    (string/replace value #"\$" "")))

(defn remove-empty
  "Given a row (of strings), remove all of the empty strings within the row"
  [row]
  (into [] (remove (fn [z]
                     (= (count z) 0)) row)))

(defn convert-to-key-format
  "Given a string, replace all that is deemed bad with a dash (:, \\, /, ws) -> -"
  [s]
  (clojure.string/replace (clojure.string/replace s #":" "") #"[/\ +]" "-"))

(defn keywordize-map-keys
  "Given a 'map', create key-value pairs and put them into a map."
  [m]
  (let [f (fn [[key val]]
            (if (string? key)
              [(keyword (string/lower-case (convert-to-key-format key))) val]
              [key val]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x))
              m)))

(defn keywordize-string
  "Given a string, convert that string into proper key format"
  [s]
  (keyword (string/lower-case (convert-to-key-format s))))

(defn nlc-keys?
  "Given a string (pdf contents), return true if it contains each key (nlc keys) that will be needed to construct an IIF, false otherwise"
  [s]
  (every? identity (map (fn [field] (string/includes? s field)) required-nlc-fields)))

(defn contractor-keys?
  "Given a string (pdf contents), return true if it contains each key (contractor keys) that will be needed to construct an IIF, false otherwise"
  [s]
  (every? identity (map (fn [field] (string/includes? s field)) required-contractor-fields)))

(defn includes-all-in?
  "Given required-fields and a string, check that the string includes all of the required-fields"
  [required-fields s]
  (every? identity (map (fn [field] (string/includes? s field)) required-fields)))

(defn name-to-key
  [name]
  (keywordize-string (str name "-last-invoice-number")))

(defn row-to-map
  "Given a row (of strings), convert it into a map by partitioning the row, vectorizing it, mapping it, and keywordizing it"
  [row]
  (let [paired-row (partition 2 row)
        vectorized-pairs (map vec paired-row)
        mapped-pairs (into {} vectorized-pairs)
        keywordized-mapped-pairs (keywordize-map-keys mapped-pairs)]
    keywordized-mapped-pairs))

(defn parse-csv-to-if
  "Given a csv filename, parse the csv's contents, remove the white space, remove the empty rows, vectorize it, convert the rows to a map, take the useful stuff, and convert it into a single map"
  [filename]
  (let [contents (read-csv filename)
        empty-strings-removed (map remove-empty contents)
        empty-rows-removed (remove empty? empty-strings-removed)
        vectorized (into [] empty-rows-removed)
        vector-of-rows (mapv row-to-map vectorized)
        useful-rows (remove empty? vector-of-rows)
        final-form (into {} useful-rows)]
    final-form))

;;finder/getter/checker functions

(defn find-x-given-regex-and-string
  "Given a string and a regex, find the information indicated by the regex -- Only useful in the format found by extract/text of invoices"
  [regex string]
  (string/trim (last (first (re-seq regex string)))))

(defn find-nlc-name
  "Given a string, find the NLC Name -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"NLC Name:(.*)Invoice No:" string))

(defn find-nlc-address
  "Given a string, find the NLC Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (try
    (string/trim (str (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 1)
                         (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 3)))
    (catch Exception e (str "Only one line of address found--" (.getMessage e)))))

(defn find-nlc-address1
  "Given a string, find the NLC Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (string/trim (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 1)))

(defn find-nlc-address2
  "Given a string, find the NLC Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (try
    (string/trim (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 3))
    (catch Exception e (str "Second line of address not found--" (.getMessage e)))))

(defn find-nlc-phone
  "Given a string, find the NLC Phone -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"NLC Phone:(.*)\r" string))

(defn find-contr-name
  "Given a string, find the Contr Name -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Name:(.*)Invoice No:" string))

(defn find-contr-address
  "Given a string, find the Contr Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Address:(.*)Invoice Date:" string))

(defn find-contr-phone
  "Given a string, find the Contr Name -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Phone:(.*)\r" string))

(defn find-invoice-date
  "Given a string, find the Invoice Date -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Invoice Date:(.*)\r" string))

(defn find-invoice-no
  "Given a string, find the Invoice Number -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Invoice No:(.*)\r" string))

(defn find-taxonomy-projects
  "Given a string, find the Taxonomy/Projects -- Only useful in the format found by extract/text of invoices"
  [string]
  (try
    (find-x-given-regex-and-string #"Taxonomy/Projects:(.*)Billing Department" string)
  (catch Exception e
    (find-x-given-regex-and-string #"Taxonomy/Projects:(.*)\r" string))))

(defn find-billing-department
  "Given a string, find the Billing Department -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Billing Department(.*)\r" string))

(defn find-total-due
  "Given a string, find the Total Due (Contractor) -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Total Due:(.*)\r" string))

(defn find-total-amount
  "Given a string, find the Total Amount (NLC) -- Only useful in the format found by extract/text of invoices"
  [string]
  (find-x-given-regex-and-string #"Amount Total:(.*)\r" string))

(defn find-client-name
  "Given a filename, extract the client-name from the filename -- Only useful in the client invoice file format"
  [filename]
  (find-x-given-regex-and-string #"billing-report-(.*)-2" filename))

(defn find-invoice-type
  "given a collection, find the invoice type of the collection by checking for the existence of certain fields"
  [coll]
  (if (or (string/includes? coll "Taxonomy/Projects") (:taxonomy-projects coll))
    "NLC"
    (if (:client-name coll)
      "INV"
      "Contractor")))

 (defn is-NLC?
   "Given a collection, return true if the invoice type of the collection is NLC"
   [coll]
   (if (= (find-invoice-type coll) "NLC")
     true
     false))

(defn is-Contractor?
  "Given a collection, return true if the invoice type of the collection is Contractor"
  [coll]
  (if (= (find-invoice-type coll) "Contractor")
    true
    false))

(defn is-Invoice?
  "Given a collection, return true if the invoice type of the collection is INV"
  [coll]
  (if (= (find-invoice-type coll) "INV")
    true
    false))

(defn check-client-name-list
  "Return the contents (vector) found in client-name-list.txt"
  []
  (edn/read-string (slurp "client-name-list.txt")))

(defn check-nlc-name-list
  "Return the contents (vector) found in nlc-name-list.txt"
  []
  (edn/read-string (slurp "nlc-name-list.txt")))

(defn check-contractor-name-list
  "Return the contents (vector) found in contractor-name-list.txt"
  []
  (edn/read-string (slurp "contractor-name-list.txt")))

(defn find-date-format
  "Given a date, determines the date-format of the date -- MM/DD/YYYY or YYYY-MM-DD -- these are the acceptable date formats for IIF"
  [date]
  (let [split-date (string/split date #"[/-]")
        date-format (if (or (= (count (first split-date)) 2) (= (count (first split-date)) 1) )
                      "MM/DD/YYYY"
                      "YYYY-MM-DD")]
    date-format))

(defn get-year-given-date
  "Given a date, find the year"
  [date]
  (let [updated-date (if (= (find-date-format date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        year (if (= (find-date-format date) "MM/DD/YYYY")
               (get split-date 2)
               (get split-date 0))]
    year))

(defn get-month-given-date
  "Given a date, find the month"
  [date]
  (let [updated-date (if (= (find-date-format date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        month (if (= (find-date-format date) "MM/DD/YYYY")
                (get split-date 0)
                (get split-date 1))]
    month))

(defn get-day-given-date
  "Given a date, find the day"
  [date]
  (let [updated-date (if (= (find-date-format date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        day (if (= (find-date-format date) "MM/DD/YYYY")
              (get split-date 1)
              (get split-date 2))]
    day))

(defn update-begin-date
  [form]
  (if (= (find-date-format (first (string/split (:begin-date form) #" "))) "MM/DD/YYYY")
    (first (string/split (:begin-date form) #" "))
    (:begin-date (assoc form :begin-date (string/replace (first (string/split (:begin-date form) #" ")) #"/" "-")))))

(defn update-reported-date
  [form]
  (if (= (find-date-format (first (string/split (:report-generated form) #" "))) "MM/DD/YYYY")
    (first (string/split (:report-generated form) #" "))
    (:report-generated (assoc form :report-generated (string/replace (first (string/split (:report-generated form) #" ")) #"/" "-")))))

(defn update-end-date
  [form]
  (if (= (find-date-format (first (string/split (:end-date form) #" "))) "MM/DD/YYYY")
    (first (string/split (:end-date form) #" "))
    (:end-date (assoc form :end-date (string/replace (first (string/split (:end-date form) #" ")) #"/" "-")))))

(defn update-fee-type
  [form]
  (if (:one-time-fee-credit-set-up-fee form)
    "Setup"
    "MC"))

(defn coll-to-key
  [coll]
  (if (is-Invoice? coll)
    (name-to-key (:client-name coll))
    (if (is-NLC? coll)
      (name-to-key (:nlc-name coll))
      (if (is-Contractor? coll)
        (name-to-key (:name coll))
        (println "Error -- no transaction type found")))))

;;functions to store, retrieve, and manipulate info from data

(defn client-to-new-entry
  "Given a collection (client, employee, contractor), create a new entry to be stored in data.txt (this part is done in add-client-to-invoice-no-map)"
  [coll]
  (if (is-Invoice? coll)
    {(name-to-key (:client-name coll)) "FIRSTENTRY1"}
    (if (is-NLC? coll)
      {(name-to-key (:nlc-name coll)) "FIRSTENTRY1"}
      (if (is-Contractor? coll)
        {(name-to-key (:name coll)) "FIRSTENTRY1"}
        (println "Error -- no transaction type found")))))

(defn update-name-list
  "Given a collection (a client), update the appropriate name-list (client-name-list.txt, nlc-name-list.txt, contractor-name-list.txt), by adding the current client's name if it does not already exist in the appropriate file"
  [coll]
  (if (is-NLC? coll)
    (if (in? (check-nlc-name-list) (:nlc-name coll))
      "Name already exists"
      (spit "nlc-name-list.txt" (conj (check-nlc-name-list) (:nlc-name coll))))
    (if (is-Contractor? coll)
      (if (in? (check-contractor-name-list) (:name coll))
        "Name already exists"
        (spit "contractor-name-list.txt" (conj (check-contractor-name-list) (:nlc-name coll))))
      (if (is-Invoice? coll)
        (if (in? (check-client-name-list) (:client-name coll))
          "Name already exists"
          (spit "client-name-list.txt" (conj (check-client-name-list) (:client-name coll))))
        "Error - no transaction type found"))))

(defn last-invoice-number-of-current-client
  "Given a collection (a client), find the last invoice number of that client from data.txt"
  [coll]
  (if (is-Invoice? coll)
    ((name-to-key (:client-name coll)) (edn/read-string (slurp "data.txt")))
    (if (is-NLC? coll)
      ((name-to-key (:nlc-name coll)) (edn/read-string (slurp "data.txt")))
      (if (is-Contractor? coll)
        ((name-to-key (:name coll)) (edn/read-string (slurp "data.txt")))
        (println "Error -- no transaction type found")))))

(defn current-client-to-invoice-no-map
  "Return the current map of client's previous invoice number map from data.txt"
  []
  (edn/read-string (slurp "data.txt")))

(defn add-client-to-invoice-no-map
  "Given a collection, add a new entry to the invoice no. map found in data.txt with the base value FIRSTENTRY1"
  [coll]
  (if (not ((coll-to-key coll) (current-client-to-invoice-no-map)))
    (spit "data.txt" (pr-str (conj (current-client-to-invoice-no-map) (client-to-new-entry coll))))
    "Client already exists in data.txt"))

(defn fresh-spit-to-file
  "Spit the base hash-maps to data.txt -- used for testing, do this in-between tests of the same invoices/bills to avoid non-unique invoice numbers"
  []
  (spit "data.txt" (pr-str (merge client-name-to-last-invoice-number nlc-name-to-last-invoice-number contractor-name-to-last-invoice-number))))

(defn update-invoice-no-to-file
  "Given a collection (invoice or bill), update the appropriate key in the hash-map located in data.txt with the current invoice number"
  [coll]
  (if (is-Invoice? coll)
    (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:client-name coll)) (:invoice-no coll))))
    (if (is-NLC? coll)
      (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:nlc-name coll)) (:invoice-no coll))))
      (if (is-Contractor? coll)
        (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:name coll)) (:invoice-no coll))))
        (println "Error -- no transaction type found")))))

(defn generate-invoice-number
  "Given a year (YYYY or YY), a month (MM), and a client-key, generate an invoice number in the format YYMMClient-code"
  [year month client-key]
  (if (= (count year) 4)
    (str (subs year 2) month ((keyword client-key) client-name-to-client-code))
    (str year month ((keyword client-key) client-name-to-client-code))))

(defn construct-nlc-map-from-pdf-contents
  "Given a string (PDF contents), construct a NLC map using finder functions IF all necessary NLC keys are found"
  [string]
  (if (nlc-keys? string)
    (let [internal-form (hash-map :nlc-name (find-nlc-name string)
                                  :nlc-address (find-nlc-address string)
                                  :nlc-address1 (find-nlc-address1 string)
                                  :nlc-address2 (find-nlc-address2 string)
                                  :nlc-phone (find-nlc-phone string)
                                  :invoice-no (find-invoice-no string)
                                  :invoice-date (find-invoice-date string)
                                  :billing-department (find-billing-department string)
                                  :taxonomy-projects (find-taxonomy-projects string)
                                  :total-due (find-total-amount string))]
      internal-form)
    (println "Error -- Insufficient Keys Found")))

(defn construct-contractor-map-from-pdf-contents
  "Given a string (PDF contents), construct a contractor map using finder functions IF all necessary contractor keys are found"
  [string]
  (if (contractor-keys? string)
    (let [internal-form (hash-map :name (find-contr-name string)
                                  :address (find-contr-address string)
                                  :phone (find-contr-phone string)
                                  :invoice-no (find-invoice-no string)
                                  :invoice-date (find-invoice-date string)
                                  :billing-department (find-billing-department string)
                                  :total-due (find-total-due string))]
      internal-form)
    (println "Error -- Insufficient Keys Found")))

(defn construct-map-from-pdf-contents
  "Given a string (PDF contents), construct a map (NLC map if a specific key is found, Contr map otherwise"
  [string]
  (if (is-NLC? string)
    (construct-nlc-map-from-pdf-contents string)
    (construct-contractor-map-from-pdf-contents string)))

;; validation functions nlc invoices

(defn unique-invoice-no?
  "Given a collection, return true if current invoice number is not the same as the previously stored invoice number in data.txt"
  [coll]
  (not= (last-invoice-number-of-current-client coll) (:invoice-no coll)))

(defn greater-invoice-no?
  "Given a collection, return true if current invoice number is greater than the previously stored invoice number in data.txt"
  [coll]
  (>  (Long/parseLong (apply str (filter #(Character/isDigit %) (:invoice-no coll)))) (Long/parseLong (apply str (filter #(Character/isDigit %) (last-invoice-number-of-current-client coll))))))

(defn validate-date
  "Given a collection, perform validation on the value associated with :invoice-date"
  [coll]
  (let [given-date (:invoice-date coll)
        updated-date (if (= (find-date-format given-date) "MM/DD/YYYY")
                       given-date
                       (:invoice-date (assoc coll :invoice-date (string/replace (:invoice-date coll) #"/" "-"))))
        month (get-month-given-date updated-date)
        day (get-day-given-date updated-date)
        year (get-year-given-date updated-date)
        validated-month (<= (Long/parseLong (clojure.string/replace month #"^0+" "")) 12)
        validated-day (<= (Long/parseLong (clojure.string/replace day #"^0+" "")) 31)
        validated-year (or (= (count year) 2) (= (count year) 4))
        validated-date (and validated-month validated-day validated-year)]
    validated-date))

;;update-stored-invoice-no (if (and validate-greater-than validate-unique)
;                                   (spit-invoice-no-to-file coll)
;                                   (println "Invoice Number is either not unique or not greater than"))
(defn validate-invoice-no
  "Given a collection, perform validation on the value associated with :invoice-no -- need to hold previous invoice no. in memory to check"
  [coll]
  (let [given-inv-no (:invoice-no coll)
        validated-char-lim (<= (count given-inv-no) 12)
        validated-no-space-test (not (string/includes? given-inv-no " "))
        validate-no-alphabetic-suffix (if (is-Invoice? coll)
                                        true
                                        (not (Character/isLetter (last given-inv-no))))
        validate-unique (unique-invoice-no? coll)
        validate-greater-than (greater-invoice-no? coll)
        validated-invoice-no (and validated-char-lim validated-no-space-test validate-no-alphabetic-suffix validate-unique validate-greater-than)]
    validated-invoice-no))

;;SEM, Setup, Development
(defn setup-fee?
  [fee-type]
  (= fee-type "Setup"))

(defn fee-type-and-client-to-quickbooks-account
  [fee-type client-name]
  (if (setup-fee? fee-type)
    (str "MC-Fees-Others-Misc:Fees-Setup:Setup-Fees-" client-name)
    (str "MC-Fees:Other Clients:" client-name)))

(defn fee-type-and-client-to-quickbooks-item
  [fee-type client-name]
  (if (setup-fee? fee-type)
    (str client-name ":Setup Fees")
    (str client-name ":MC Fees")))

(defn currency-and-client-to-accounts-receivable
  [currency-type client-name]
  (str "Accounts Receivable-" currency-type ":" client-name "-A/R"))

(def valid-billing-departments
  ["Taxonomy"
   "Engineering"
   "Product"
   "Marketing"
   "Client Engagement"
   "BD"
   "Finance"
   "People & Culture"
   "Corp IT"
   "Corporate"
   "R&D"])

(defn validate-billing-dept
  "Given a collection, check if the value associated with :billing-department key matches one of the valid billing departments"
  [coll]
  (let [given-bd (:billing-department coll)
        validated-bd (in? valid-billing-departments given-bd)]
    validated-bd))

(def valid-taxonomies
  ["Special_Projects"
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
   "Swedish_Sweden"])

(defn validate-taxonomy
  "Given a collection, check if the value associated with :taxonomy-projects key matches one of the valid taxonomies/projects"
  [coll]
  (let [given-taxonomy (:taxonomy-projects coll)
        validated-taxonomy (in? valid-taxonomies given-taxonomy)]
    validated-taxonomy))

(defn validate-nlc-name
  "Given a collection, make sure the value associated with :nlc-name key is not empty"
  [coll]
  (let [given-name (:nlc-name coll)
        validated-nlc-name (> (count given-name) 0)]
    validated-nlc-name))

(defn validate-nlc-address
  "Given a collection, make sure the value associated with :nlc-address key is not empty"
  [coll]
  (let [given-address (:nlc-address coll)
        validated-nlc-address (> (count given-address) 0)]
    validated-nlc-address))

(defn validate-nlc-phone
  "Given a collection, make sure the value associated with :nlc-phone key has a count of 10 or 11 (if leading number is 1)"
  [coll]
  (let [given-phone (:nlc-phone coll)
        phone-number (string/replace given-phone #"[ +.-]" "")
        validated-nlc-phone (or (= (count phone-number) 10) (and (= (count phone-number) 11) (string/starts-with? phone-number "1")))]
    validated-nlc-phone))

(defn validate-nlc-invoice
  "Given a collection, run all validation functions, print the result of all validation functions, and return true if all validation functions return true, false otherwise"
  [coll]
  (println "Validation for invoice:" (:nlc-name coll))
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
  "Given a collection, make sure the value associated with :name key is not empty"
  [coll]
  (let [given-name (:name coll)
        validated-contr-name (> (count given-name) 0)]
    validated-contr-name))

(defn validate-contr-address
  "Given a collection, make sure the value associated with :address key is not empty"
  [coll]
  (let [given-address (:address coll)
        validated-contr-address (> (count given-address) 0)]
    validated-contr-address))

(defn validate-contr-phone
  "Given a collection, make sure the value associated with :phone key has a count of 10 or 11 (if leading number is 1)"
  [coll]
  (let [given-phone (:phone coll)
        phone-number (string/replace given-phone #"[-+]" "")
        validated-contr-phone (or (= (count phone-number) 10) (and (= (count phone-number) 11) (string/starts-with? phone-number "1")))]
    validated-contr-phone))

(defn validate-contr-invoice
  "Returns True if all validations succeed, else false"
  [coll]
  (println "Validation for invoice:" (:name coll))
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

(defn validate-invoice
  [coll]
  (println "Validation for invoice:" (:name coll))
  (println "Name:" (validate-contr-name coll))
  (println "Date:" (validate-date coll))
  (println "Invoice No:" (validate-invoice-no coll))
  (and (validate-contr-name coll)
       (validate-date coll)
       (validate-invoice-no coll)))

(defn validate-transaction
  "Given a form (hashmap), validate-nlc-invoice if the form has the appropriate key, and validate-contr-invoice otherwise"
  [form]
  (if (is-NLC? form)
    (validate-nlc-invoice form)
    (if (is-Contractor? form)
      (validate-contr-invoice form)
      (if (is-Invoice? form)
        (validate-invoice form)
        (println "Error -- No Transaction Type Found")))))


;; IIF construction functions

(defn construct-iif-headers-for-bill
  "Constructs the string that will be used as the headers in a properly formatted IIF file for Bills"
  []
  (str "!TRNS\tTRNSID\tTRNSTYPE\tDATE\tACCNT\tNAME\tCLASS\tAMOUNT\tDOCNUM\tMEMO\tCLEAR\tTOPRINT\tADDR1\tADDR2\tDUEDATE\tTERMS"
       "\n!SPL\tSPLID\tTRNSTYPE\tDATE\tACCNT\tNAME\tCLASS\tAMOUNT\tDOCNUM\tMEMO\tCLEAR\tQNTY\tREIMBEXP\tSERVICEDATE"
       "\n!ENDTRNS\n"))


(defn construct-iif-trns-for-contractor-bill
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "TRNS\t\tBILL\t"
       (:invoice-date coll) "\t"
       "Accounts Payable\t"
       (:name coll) "\t\t"
       "-" (validize-total-due (:total-due coll)) "\t"
       (:invoice-no coll) "\t\t\t\t"
       (:address coll) "\t\t\n"))

(defn construct-iif-spl-for-contractor-bill
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "SPL\t\tBILL\t"
       (:invoice-date coll) "\t"
       ((keywordize-string (:billing-department coll)) billing-department-to-quickbooks-account) "\t"
       "\t\t"
       (validize-total-due (:total-due coll)) "\t\t\t\t\t"
       "\t\t\n"
       "ENDTRNS\n\n"))


;; construct iif from nlc invoices

(defn construct-iif-trns-for-nlc-bill
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "TRNS\t\tBILL\t"
       (:invoice-date coll) "\t"
       "Accounts Payable" "\t"
       (:nlc-name coll) "\t\t"
       "-" (validize-total-due (:total-due coll)) "\t"
       (:invoice-no coll) "\t\t\t\t"
       (:nlc-address1 coll) "\t"
       (:nlc-address2 coll) "\t\n"))

(defn construct-iif-spl-for-nlc-bill
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "SPL\t\tBILL\t"
       (:invoice-date coll) "\t"
       (if (= (:billing-department coll) "Taxonomy")
         (str ((keywordize-string (:taxonomy-projects coll)) (:taxonomy-projects billing-department-to-quickbooks-account)) (:nlc-name coll) "-dev")
         ((keywordize-string (:billing-department coll)) billing-department-to-quickbooks-account)) "\t"
       "\t\t"
       (string/replace (:total-due coll) #"\$" "") "\t\t\t\t\t"
       "\t\t\n"
       "ENDTRNS\n\n"))

;; IIF Construction for Invoices

(defn construct-iif-headers-for-invoice
  "Constructs the string that will be used as the headers in a properly formatted IIF file for Invoices"
  []
  (str "!TRNS\tTRNSID\tTRNSTYPE\tDATE\tACCNT\tNAME\tCLASS\tAMOUNT\tDOCNUM\tMEMO\tCLEAR\tTOPRINT\tADDR1\tDUEDATE\tTERMS"
       "\n!SPL\tSPLID\tTRNSTYPE\tDATE\tACCNT\tNAME\tCLASS\tAMOUNT\tDOCNUM\tMEMO\tCLEAR\tQNTY\tINVITEM\tSERVICEDATE"
       "\n!ENDTRNS\n"))

(defn construct-iif-trns-for-invoice
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "TRNS\t\tINVOICE\t"
       (:end-date coll) "\t"
       (currency-and-client-to-accounts-receivable (:currency coll) (:client-name coll)) "\t"
       (:name coll) "\t\t"
       (validize-total-due (:total-due coll)) "\t"
       (:invoice-no coll) "\t\t\t\t\t\t"
       "\t\t"
       "\t\n"))


(defn construct-iif-spl-for-invoice
  "Given a collection, construct the string that will be used as the information in a properly formatted IIF file"
  [coll]
  (str "SPL\t\tINVOICE\t"
       (:end-date coll) "\t"
       (fee-type-and-client-to-quickbooks-account (:fee-type coll) (:client-name coll)) "\t"
       (:name coll) "\t\t"
       "-" (validize-total-due (:total-due coll)) "\t\t\t\t\t"
       (fee-type-and-client-to-quickbooks-item (:fee-type coll) (:name coll)) "\t\n"
       "ENDTRNS\n\n"))

;; Builder Functions -- functions in the steps of the Master Functions

(defn pdf-filename-to-internal-form
  "Given a filename (PDF), parse a pdf file's contents, and construct a map from those contents"
  [filename]
  (let [contents (read-pdf filename)
        internal-form (construct-map-from-pdf-contents contents)]
    internal-form))

(defn internal-form-to-iif-writer
  "Given a writer and a form (hashmap), write using NLC format if the form has the appropriate key, write using Contr format otherwise."
  [writer form]
  (try
    (if (is-NLC? form)
      (.write writer (str (construct-iif-headers-for-bill)
                          (construct-iif-trns-for-nlc-bill form)
                          (construct-iif-spl-for-nlc-bill form)))
      (.write writer (str (construct-iif-headers-for-bill)
                          (construct-iif-trns-for-contractor-bill form)
                          (construct-iif-spl-for-contractor-bill form))))
    true
    (catch Exception e
      (println "Exception Error Writing")
      false)))

(defn pdf-filename-to-iif-writer
  "Given a filename (pdf) and a writer, convert from pdf-filename to internal form, run validation on internal form, write to iif from internal form. Return true if each step is completed, return errors otherwise"
  [pdf-filename writer]
  (if-let [internal-form (pdf-filename-to-internal-form pdf-filename)]
    (if-let [validated-if (do (add-client-to-invoice-no-map internal-form) (validate-transaction internal-form))]
      (if-let [constructed-iif (internal-form-to-iif-writer writer internal-form)]
        (update-invoice-no-to-file internal-form)
        (println "Writer failed"))
      (println "Validation failed"))
    (println "Parser Failed")))

(defn internal-form-to-iif-writer-for-invoices
  "Given a writer and a form (hashmap), write using NLC format if the form has the appropriate key, write using Contr format otherwise."
  [writer form]
  (try
    (if (is-Invoice? form)
      (.write writer (str (construct-iif-headers-for-invoice)
                          (construct-iif-trns-for-invoice form)
                          (construct-iif-spl-for-invoice form))))
    true
    (catch Exception e
      (println "Exception Error Writing")
      false)))

(defn internal-form-to-invoice-form
  "Given the 'standard' internal form, convert to a form suitable for invoices deemed invoice form"
  [form]
  (hash-map :total-due (:invoice-total form)
            :name (:pricing-plan form)
            :client-name (:client-name form)
            :currency (:currency form)
            :monthly-minimum-fee (:monthly-minimum-fee form)
            :invoice-date (update-begin-date form)
            :end-date (update-end-date form)
            :reported-date (update-reported-date form)
            :fee-type (update-fee-type form)
            :invoice-no (generate-invoice-number (get-year-given-date (update-begin-date form)) (get-month-given-date (update-begin-date form)) (:client-name form))))

(defn invoice-to-internal-form
  "Given a csv filename, convert its contents into a useful internal form (a hash-map)"
  [csv-filename]
  (let [contents (read-csv csv-filename)
        added-client-name (conj contents ["Client Name" (find-client-name csv-filename)])
        empty-strings-removed (map remove-empty added-client-name)
        empty-rows-removed (remove empty? empty-strings-removed)
        vectorized (into [] empty-rows-removed)
        vector-of-rows (mapv row-to-map vectorized)
        useful-rows (remove empty? vector-of-rows)
        internal-form (into {} useful-rows)
        invoice-form (internal-form-to-invoice-form internal-form)]
    invoice-form))

(defn invoice-filename-to-iif-writer
  "Given a filename (pdf) and a writer, convert from pdf-filename to internal form, run validation on internal form, write to iif from internal form. Return true if each step is completed, return errors otherwise"
  [invoice-filename writer]
  (if-let [internal-form (invoice-to-internal-form invoice-filename)]
    (if-let [validated-if (do (add-client-to-invoice-no-map internal-form) (validate-transaction internal-form))]
      (if-let [constructed-iif (internal-form-to-iif-writer-for-invoices writer internal-form)]
        (update-invoice-no-to-file internal-form)
        (println "Writer failed"))
      (println "Validation failed"))
    (println "Parser Failed")))



;; Master Functions -- CSV file gets parsed into a hashmap. That hashmap goes through validation
;; checks. Then, an iif file is constructed using that hashmap.

(defn pdf-dir-to-iif-file
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [pdf-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [pdf-filename] (with-open [r (io/reader (str pdf-dirname "/" pdf-filename))]
                               (pdf-filename-to-iif-writer (str pdf-dirname "/" pdf-filename) w)))
          (list-filenames pdf-dirname))))

(defn invoice-dir-to-iif-file
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [invoice-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [invoice-filename] (with-open [r (io/reader (str invoice-dirname "/" invoice-filename))]
                               (invoice-filename-to-iif-writer (str invoice-dirname "/" invoice-filename) w)))
          (list-filenames invoice-dirname))))

;;Currently bills are in PDF format and Invoices are in CSV format. The functions below allow the reverse to be converted to IIF if things change
;;Be careful though! These functions aren't identical to the above master functions, as I stopped updating them when I knew I didn't actively need them

;;Builder functions for the (currently) unused master functions

(defn csv-reader-to-internal-form
  "Given a reader, parse a csv file's contents, remove white speace, remove empty rows, vectorize, mappize, and return the final form (a hashmap)"
  [reader]
  (let [contents (into [] (doall (csv/read-csv reader)))
        empty-strings-removed (map remove-empty contents)
        empty-rows-removed (remove empty? empty-strings-removed)
        vectorized (into [] empty-rows-removed)
        vector-of-rows (mapv row-to-map vectorized)
        useful-rows (remove empty? vector-of-rows)
        final-form (into {} (into [] useful-rows))]
    final-form))

(defn csv-reader-to-iif-writer
  "Given a reader and a writer, convert from csv-reader to internal form, run validation on internal form, write to iif from internal form. Return true if each step is completed, return errors otherwise"
  [reader writer]
  (if-let [internal-form (csv-reader-to-internal-form reader)]
    (if-let [validated-if (validate-transaction internal-form)]
      (if-let [constructed-iif (internal-form-to-iif-writer writer internal-form)]
        true
        (println "Writer failed"))
      (println "Validation failed"))
    (println "Reader failed")))

(defn pdf-filename-to-iif-writer-for-invoices
  "Given a filename (pdf) and a writer, convert from pdf-filename to internal form, run validation on internal form, write to iif from internal form. Return true if each step is completed, return errors otherwise"
  [pdf-filename writer]
  (if-let [internal-form (pdf-filename-to-internal-form pdf-filename)]
    (if-let [validated-if (do (add-client-to-invoice-no-map internal-form) (validate-transaction internal-form))]
      (if-let [constructed-iif (internal-form-to-iif-writer-for-invoices writer internal-form)]
        (update-invoice-no-to-file internal-form)
        (println "Writer failed"))
      (println "Validation failed"))
    (println "Parser Failed")))

;;(currently) unused Master Functions

(defn csv-dir-to-iif-file
  "Given a csv-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [csv-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [csv-filename] (with-open [r (io/reader csv-filename)]
                               (csv-reader-to-iif-writer r w)))
          (list-filenames csv-dirname))))

(defn pdf-dir-to-iif-file-for-invoices
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [pdf-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [pdf-filename] (with-open [r (io/reader (str pdf-dirname "/" pdf-filename))]
                               (pdf-filename-to-iif-writer-for-invoices (str pdf-dirname "/" pdf-filename) w)))
          (list-filenames pdf-dirname))))

;;Tester functions -- potentially useful functions for testing purposes

(defn pdf-file-to-iif-file
  "Given a pdf-filename and iif-filename, open a writer and write to an iif file"
  [pdf-filename iif-filename]
  (with-open [w (io/writer iif-filename)]
    (pdf-filename-to-iif-writer pdf-filename w)))

(defn csv-file-to-iif-file
  "Given a csv-filename and iif-filename, open a writer and reader and convert from a csv-reader to an iif-writer"
  [csv-filename iif-filename]
  (with-open [r (io/reader csv-filename)
              w (io/writer iif-filename)]
    (csv-reader-to-iif-writer r w)))


(comment
  (def pdf-contents (read-pdf "nlc-pdf-invoice-example.pdf"))
  (def csv-contents (read-csv "nlc-invoice-example.csv"))
  (def pdf-dir "C:/Users/Steve/IdeaProjects/parse_csv/bills")
  (def csv-dir "C:/Users/Steve/IdeaProjects/parse_csv/invoices")
  (def csv-test-file (str csv-dir "/" (first (list-filenames "C:/Users/Steve/IdeaProjects/parse_csv/invoices"))))
  (def pdf-test-file (str pdf-dir "/" (first (list-filenames "C:/Users/Steve/IdeaProjects/parse_csv/bills"))))
  )


