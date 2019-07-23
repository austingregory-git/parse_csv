(ns parse-csv.core
  (:require [clojure.java.io :as io]
            [clojure.data.csv :as csv]
            [clojure.walk :as walk]
            [clojure.string :as string]
            [clojure.walk :refer [postwalk]]
            [pdfboxing.text :as text]
            [clojure.edn :as edn]
    ;; [java-time :as time]
    ))



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

(defn list-filenames
  "Given a filepath, list all file names of the given directory"
  [path]
  (seq (.list (clojure.java.io/file path))))

(defn in?
  "true if coll contains elm"
  [coll elm]
  (some #(= elm %) coll))

(defn validize-total-due
  "Given a value which represents a total amount "
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

;;convert-to-key-format
(defn replace-bad-with-dash
  "Given a string, replace all that is deemed bad with a dash (:, \\, /, ws) -> -"
  [string]
  (clojure.string/replace (clojure.string/replace string #":" "") #"[/\ +]" "-"))

;;keywordize-map-keys
(defn keywordize
  "Given a 'map', create key-value pairs and put them into a map."
  [m]
  (let [f (fn [[key val]]
            (if (string? key)
              [(keyword (string/lower-case (replace-bad-with-dash key))) val]
              [key val]))]
    (postwalk (fn [x] (if (map? x) (into {} (map f x)) x))
              m)))

(defn keywordize-string
  "Given a string, convert that string into a key with no bad characters"
  [string]
  (keyword (string/lower-case (replace-bad-with-dash string))))

(def required-nlc-fields
  ["NLC Name" "NLC Address" "NLC Phone" "Taxonomy/Projects" "Billing Department" "Amount Total"])

(def required-contractor-fields
  ["Name" "Address" "Phone" "Billing Department" "Total Due"])

(defn nlc-keys?
  "Given a string (pdf contents), return true if it contains each key (nlc keys) that will be needed to construct an IIF, false otherwise"
  [string]
  (every? identity (map (fn [field] (string/includes? string field)) required-nlc-fields)))

(defn contractor-keys?
  "Given a string (pdf contents), return true if it contains each key (contractor keys) that will be needed to construct an IIF, false otherwise"
  [string]
  (every? identity (map (fn [field] (string/includes? string field)) required-contractor-fields)))

(defn includes-all-in?
  [required-fields string]
  (every? identity (map (fn [field] (string/includes? string field)) required-fields)))

(defn row-to-map
  "Given a row (of strings), convert it into a map by partitioning the row, vectorizing it, mapping it, and keywordizing it"
  [row]
  (let [paired-row (partition 2 row)
        vectorized-pairs (map vec paired-row)
        mapped-pairs (into {} vectorized-pairs)
        keywordized-mapped-pairs (keywordize mapped-pairs)]
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
  (string/trim (str (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 1)
                    (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 3))))

(defn find-nlc-address1
  "Given a string, find the NLC Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (string/trim (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 1)))

(defn find-nlc-address2
  "Given a string, find the NLC Address -- Only useful in the format found by extract/text of invoices"
  [string]
  (string/trim (get (first (re-seq #"NLC Address:(.*)Invoice Date:(.*)\r\n(.*)\r" string)) 3)))

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
  [filename]
  (find-x-given-regex-and-string #"billing-report-(.*)-2" filename))


(defn convert-billing-dept-to-account-name-for-iif
  "Given a string (representing a value in a map, convert to a valid account name used in IIF format"
  [string]
  (str string ":" string " Expenses"))

(def optional-nlc-fields
  [])

(def nlc-invoice-fields
  (into [] (concat required-nlc-fields optional-nlc-fields)))

(defn invoice-type?
  [coll]
  (if (or (string/includes? coll "Taxonomy/Projects") (:taxonomy-projects coll))
    "NLC"
    (if (:client-name coll)
      "INV"
      "Contractor")))

 (defn is-NLC?
   [coll]
   (if (= (invoice-type? coll) "NLC")
     true
     false))

(defn is-Contractor?
  [coll]
  (if (= (invoice-type? coll) "Contractor")
    true
    false))

(defn is-Invoice?
  [coll]
  (if (= (invoice-type? coll) "INV")
    true
    false))

;; Hash-map for client-name as a key which gives the client-code to be used to generate an invoice number
(def client-name-to-client-code
  (hash-map :liturgicalpub "litpub"
            :zipch "zipch"))

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

(defn check-client-name-list
  []
  (edn/read-string (slurp "client-name-list.txt")))

(defn check-nlc-name-list
  []
  (edn/read-string (slurp "nlc-name-list.txt")))

(defn check-contractor-name-list
  []
  (edn/read-string (slurp "contractor-name-list.txt")))

(defn name-to-key
  [name]
  (keywordize-string (str name "-last-invoice-number")))

(defn client-to-new-entry
  [coll]
  (if (is-Invoice? coll)
    {(name-to-key (:client-name coll)) "FIRSTENTRY1"}
    (if (is-NLC? coll)
      {(name-to-key (:nlc-name coll)) "FIRSTENTRY1"}
      (if (is-Contractor? coll)
        {(name-to-key (:name coll)) "FIRSTENTRY1"}
        (println "Error -- no transaction type found")))))


(defn update-name-list
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
  [coll]
  (if (is-Invoice? coll)
    ((name-to-key (:client-name coll)) (edn/read-string (slurp "data.txt")))
    (if (is-NLC? coll)
      ((name-to-key (:nlc-name coll)) (edn/read-string (slurp "data.txt")))
      (if (is-Contractor? coll)
        ((name-to-key (:name coll)) (edn/read-string (slurp "data.txt")))
        (println "Error -- no transaction type found")))))

(defn current-client-to-invoice-no-map
  []
  (edn/read-string (slurp "data.txt")))

(defn add-client-to-invoice-no-map
  "Given a collection, add a new entry to the invoice no. map found in data.txt with the base value FIRSTENTRY1"
  [coll]
  (if (is-Invoice? coll)
    (spit "data.txt" (pr-str (conj (current-client-to-invoice-no-map) (client-to-new-entry coll))))
    (if (is-NLC? coll)
      (spit "data.txt" (pr-str (conj (current-client-to-invoice-no-map) (client-to-new-entry coll))))
      (if (is-Contractor? coll)
        (spit "data.txt" (pr-str (conj (current-client-to-invoice-no-map) (client-to-new-entry coll))))
        (println "Error -- no transaction type found")))))

(defn fresh-spit-to-file
  []
  (spit "data.txt" (pr-str (merge client-name-to-last-invoice-number nlc-name-to-last-invoice-number contractor-name-to-last-invoice-number))))

(defn update-invoice-no-to-file
  [coll]
  (if (is-Invoice? coll)
    (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:client-name coll)) (:invoice-no coll))))
    (if (is-NLC? coll)
      (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:nlc-name coll)) (:invoice-no coll))))
      (if (is-Contractor? coll)
        (spit "data.txt" (pr-str (assoc (current-client-to-invoice-no-map) (name-to-key (:name coll)) (:invoice-no coll))))
        (println "Error -- no transaction type found")))))

(defn unique-invoice-no?
  "Given a collection, return true if current invoice number is not the same as the previously stored invoice number in data.txt"
  [coll]
  (not= (last-invoice-number-of-current-client coll) (:invoice-no coll)))

(defn greater-invoice-no?
  "Given a collection, return true if current invoice number is greater than the previously stored invoice number in data.txt"
  [coll]
  (>  (Long/parseLong (apply str (filter #(Character/isDigit %) (:invoice-no coll)))) (Long/parseLong (apply str (filter #(Character/isDigit %) (last-invoice-number-of-current-client coll))))))

(defn generate-invoice-number
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

(defn construct-contr-map-from-pdf-contents
  "Given a string (PDF contents), construct a Contr map using finder functions IF all necessary Contr keys are found"
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
    (construct-contr-map-from-pdf-contents string)))

(defn parse-pdf-to-if
  "Given a filename (PDF), parse the PDF's contents and construct a map from those contents"
  [filename]
  (let [contents (read-pdf filename)
        internal-form (construct-map-from-pdf-contents contents)]
    internal-form))

;; validate nlc invoices

(defn date-format?
  [date]
  (let [split-date (string/split date #"[/-]")
        date-format (if (or (= (count (first split-date)) 2) (= (count (first split-date)) 1) )
          "MM/DD/YYYY"
          "YYYY-MM-DD")]
    date-format))

(defn get-year-given-date
  [date]
  (let [updated-date (if (= (date-format? date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        year (if (= (date-format? date) "MM/DD/YYYY")
                (get split-date 2)
                (get split-date 0))]
    year))

(defn get-month-given-date
  [date]
  (let [updated-date (if (= (date-format? date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        month (if (= (date-format? date) "MM/DD/YYYY")
               (get split-date 0)
               (get split-date 1))]
    month))

(defn get-day-given-date
  [date]
  (let [updated-date (if (= (date-format? date) "MM/DD/YYYY")
                       date
                       (string/replace date #"/" "-"))
        split-date (string/split updated-date #"[/-]")
        day (if (= (date-format? date) "MM/DD/YYYY")
               (get split-date 1)
               (get split-date 2))]
    day))

(defn validate-date
  "Given a collection, perform validation on the value associated with :invoice-date"
  [coll]
  (let [given-date (:invoice-date coll)
        updated-date (if (= (date-format? given-date) "MM/DD/YYYY")
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

(def accounts-receivable
  {:liturgicalpub "Accounts Receivable-USD:liturgicalpub-A/R"
   :zipch "Accounts Receivable-CHF:zipch-A/R"})

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


;; Construct iif from contractor invoices

(def transaction-types
  ["BILL" "INVOICE"])

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

;; Master Functions -- CSV file gets parsed into a hashmap. That hashmap goes through validation
;; checks. Then, an iif file is constructed using that hashmap.

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

(defn pdf-file-to-iif-file
  "Given a pdf-filename and iif-filename, open a writer and write to an iif file"
  [pdf-filename iif-filename]
  (with-open [w (io/writer iif-filename)]
    (pdf-filename-to-iif-writer pdf-filename w)))


(defn pdf-dir-to-iif-file
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [pdf-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [pdf-filename] (with-open [r (io/reader (str pdf-dirname "/" pdf-filename))]
                               (pdf-filename-to-iif-writer (str pdf-dirname "/" pdf-filename) w)))
          (list-filenames pdf-dirname))))

(defn pdf-dir-to-iif-file-for-invoices
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [pdf-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [pdf-filename] (with-open [r (io/reader (str pdf-dirname "/" pdf-filename))]
                               (pdf-filename-to-iif-writer-for-invoices (str pdf-dirname "/" pdf-filename) w)))
          (list-filenames pdf-dirname))))

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

(defn csv-file-to-iif-file
  "Given a csv-filename and iif-filename, open a writer and reader and convert from a csv-reader to an iif-writer"
  [csv-filename iif-filename]
  (with-open [r (io/reader csv-filename)
              w (io/writer iif-filename)]
    (csv-reader-to-iif-writer r w)))

(defn csv-dir-to-iif-file
  "Given a csv-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [csv-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [csv-filename] (with-open [r (io/reader csv-filename)]
                               (csv-reader-to-iif-writer r w)))
          (list-filenames csv-dirname))))

(defn update-begin-date
  [form]
  (if (= (date-format? (first (string/split (:begin-date form) #" "))) "MM/DD/YYYY")
    (first (string/split (:begin-date form) #" "))
    (:begin-date (assoc form :begin-date (string/replace (first (string/split (:begin-date form) #" ")) #"/" "-")))))

(defn update-reported-date
  [form]
  (if (= (date-format? (first (string/split (:report-generated form) #" "))) "MM/DD/YYYY")
    (first (string/split (:report-generated form) #" "))
    (:report-generated (assoc form :report-generated (string/replace (first (string/split (:report-generated form) #" ")) #"/" "-")))))

(defn update-end-date
  [form]
  (if (= (date-format? (first (string/split (:end-date form) #" "))) "MM/DD/YYYY")
    (first (string/split (:end-date form) #" "))
    (:end-date (assoc form :end-date (string/replace (first (string/split (:end-date form) #" ")) #"/" "-")))))

(defn update-fee-type
  [form]
  (if (:one-time-fee-credit-set-up-fee form)
    "Setup"
    "MC"))

(defn internal-form-to-invoice-form
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

(defn invoice-dir-to-iif-file
  "Given a pdf-dirname (path) and an iif-filename, open a writer, read each file in the given directory's contents, and write to the given iif file"
  [invoice-dirname iif-filename]
  (with-open [w (io/writer iif-filename)]
    (run! (fn [invoice-filename] (with-open [r (io/reader (str invoice-dirname "/" invoice-filename))]
                               (invoice-filename-to-iif-writer (str invoice-dirname "/" invoice-filename) w)))
          (list-filenames invoice-dirname))))

(comment
  (def x (read-pdf "nlc-pdf-invoice-example.pdf"))
  (new-find-nlc-name x)
  (def pdf-dir "C:/Users/Steve/IdeaProjects/parse_csv/bills")
  (def csv-dir "C:/Users/Steve/IdeaProjects/parse_csv/invoices")
  (def test-file (str csv-dir "/" (first (list-filenames "C:/Users/Steve/IdeaProjects/parse_csv/invoices"))))
  )


