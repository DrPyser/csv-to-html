#lang racket/base

;;racket library for pattern matching (match form [pattern expr]...)
;;https://docs.racket-lang.org/reference/stx-patterns.html
(require racket/match)

;;racket library for string manipulation
;;https://docs.racket-lang.org/reference/strings.html#%28part._.Additional_.String_.Functions%29
(require racket/string)

;;racket library for command line parsing
;;https://docs.racket-lang.org/reference/Command-Line_Parsing.html
(require racket/cmdline)

;;racket library for pretty printing
(require racket/pretty)

;;racket library for list manipulation (used for group-by)
(require racket/list)

(require racket/function)
;;library used to parse a csv file into lisp data
;;https://docs.racket-lang.org/csv-reading/index.html
(require (planet neil/csv))

;; ;;library used to manipulate sxml data
;; ;;https://docs.racket-lang.org/sxml/index.html
;; (require sxml)

;;library used to write html to a file from an sxml description
;;https://docs.racket-lang.org/html-writing/index.html
(require (planet neil/html-writing))

;;the gregor library is used for date manipulation(parsing, formatting, etc)
;;https://docs.racket-lang.org/gregor/index.html
(require gregor)
(require gregor/time)

;;csv reader specs. Can specify field separator characters, and type of line separation(see csv-reader library doc)
(define reader-spec
  '((newline-type . lax)
    (separator-chars #\,)
    ))

;;A reader using the reader-spec
(define csv-reader (make-csv-reader-maker reader-spec))

;;;-----------------------------------------------parameters-------------------------------------------------------------------
(define calendar-edition (make-parameter "bilingual"))

(define html-output-port (make-parameter (current-output-port)))

;;the fields' names
(define fields-names
  (make-parameter
   (hash "location-name" "Title"
         "location-city" "_location_town"
         "location-region" "_location_region"
         "location-address" "_location_address"
         "event-region" "_location_region"
         "event-name" "Title"
         "event-abbreviation" "abbreviation"
         "location-abbreviation" "abbreviation"
         "location-id" "_location_id"
         "event-location-id" "_location_id"
         "price-range" "price-range"
         "phone-number" "event-phone-number"
         "start-date" "_event_start_date"
         "start-time" "_event_start_time"
         "print-summary-english" "print-summary-english"
         "print-summary-french" "print-summary-french")))

;;Some default information
(define defaults
  (make-parameter
   (hash "calendar-edition" "bilingual"
         "date-format" "M/d/yyyy"
         "time-format" "H:mm:ss"
         "price-range" #f
         "phone-number" #f
         "location-abbreviation" #f
         )))

;;;-----------------------------------------------------------



#|
Maps a number to a month's name in french
|#
(define (month->string/fr num)
  (case num
    [(1) "Janvier"]
    [(2) "Février"]
    [(3) "Mars"]
    [(4) "Avril"]
    [(5) "Mai"]
    [(6) "Juin"]
    [(7) "Juillet"]
    [(8) "Août"]
    [(9) "Septembre"]
    [(10) "Octobre"]
    [(11) "Novembre"]
    [(12) "Décembre"]
    ))

#|
Maps a number to a month name in english
|#
(define (month->string/en num)
  (case num
    [(1) "January"]
    [(2) "February"]
    [(3) "March"]
    [(4) "April"]
    [(5) "May"]
    [(6) "June"]
    [(7) "July"]
    [(8) "August"]
    [(9) "September"]
    [(10) "October"]
    [(11) "November"]
    [(12) "December"]
    ))

#|
returns the date object for this event
in: row: list: an event as a list of parsed fields from the csv row(should include a "start_date" field)
out: a 'date' object(see gregor library documentation)
|#

(define (get-date row)
  (let ([start-date-field (hash-ref (fields-names) "start-date")])
    (match (assoc start-date-field row)
      [(cons _ d) d]
      [#f (error (format "Missing field '~a' in row ~a" start-date-field row))]))
  )

#|
returns the month(as a number) for the event's start date
in: row: list: an event as a list of parsed fields from the csv row(should include a "start_date" field)
out: number: a number representing a month(1-12) for this event's start date
|#

(define (get-month row)
  (->month (get-date row)))

#|
returns the day(as a number) for the event's start date
in: row: list: an event as a list of parsed fields from the csv row(should include a "start_date" field)
out: number: a number between 1-31 representing the day of the month for this event's start date
|#
(define (get-day row)
  (->day (get-date row)))

#|
returns a list of months(as numbers 1-12) in which events happen, given a list of events
in: events: list: a list of events (as parsed csv rows)(should include a 'start_date' field)
out: months: list: a list of numbers representing months in which at least one events happen
|#
(define (get-months events) (remove-duplicates (map get-month events) eqv?))


#|
returns a list of day numbers in which events happen
in: rows: list : list of events: (as rows of parsed csv data)(should include a "start_date" field)
out: list: list of number representing days (1-31) in which events happen for the input set of events
|#
(define (get-days rows) (remove-duplicates (map get-day rows) eqv?))


#|
Parses the string values of an event into more useful data representation(e.g. dates and times as date and time objects)
in: row : list: association list (mapping of field to value) representing data fields for the event/location
out: list: parsed row (same as input, except with some values transformed into another representation)
|#
(define (parse-row row)
  (let ([date-field? (lambda (field) (string=? field (hash-ref (fields-names) "start-date")))]
        [time-field? (lambda (field) (string=? field (hash-ref (fields-names) "start-time")))]
        [date-field (hash-ref (fields-names) "start-date")]
        [time-field (hash-ref (fields-names) "start-time")]
        [date-format (hash-ref (defaults) "date-format")]
        [time-format (hash-ref (defaults) "time-format")])
    (map (lambda (field)
           (match field
             ;;parse start-date field as a date object(using gregor library)
             [(cons (? date-field?) date-str)
              ;;if there is no date string, current date is used
              (if (= (string-length date-str) 0) (cons date-field (today))
                  (cons date-field (parse-date date-str date-format)))]
             ;;parse start-time field as a time object(using gregor library)
             [(cons (? time-field?) time-str)
              ;;if there is no time string, current time is used
              (if (= (string-length time-str) 0) (cons time-field (current-time))
                  (cons time-field (parse-time time-str time-format)))]
             [(cons name "") (cons name #f)];empty fields are replaced by the 'false' racket value
             [_ field];otherwise the field is left untouched
             )) row))
  )


#|
sxml generator for the whole document.
in: months: list: list of sxml data for each months in the calendar listing
in: title: string: title of the document
out: sxml document for the calendar listing
|#
(define (gen-document-sxml months title)
  `(*TOP*
    (html
     (head
      (meta (@ (charset "utf8"))) (title ,title)
      (style
       ".events-list {list-style-type: none; margin:0; padding: 0;}"
       ".day h3 {color:#1d328b;}"
       ".month h2 {color:#1d328b;}"
       ".event-marker {font-family: FFDingbests;}"
       ".event-info {display:inline-block;}"
       ".location-info {margin: 0;}"))
     (body ,@months))))

#|
sxml generator for a month in the bilingual calendar
in: month-en : string : name of the month in english
in: month-fr : string : name of the month in french
in: days : list : list of sxml data for each days in the month
out:  resulting sxml for the month
|#
(define (gen-month-sxml/bilingual month-en month-fr days)
  `(div (@ (class "month"))
        (h2 ,(string-upcase month-en) " - " ,(string-upcase month-fr))
        ,@days))

#|
Template for a day in the bilingual calendar
in: day-fr: string: day of the week in french
in: day-en: string: day of the week in english
in: day-num: number: day of the month as a number(1-31)
in: events: list: list of sxml data for the events on that day
out: sxml data for the day
|#
(define (gen-day-sxml/bilingual day-fr day-num day-en events-sxml)
  `(div (@ (class "day"))
        (h3 ,(string-downcase day-fr) " " ,(number->string day-num) " " ,(string-titlecase day-en))
        (ul (@ (class "events-list")) ,@events-sxml)))

#|
template for an event in the bilingual calendar
in: event: a association list representing the fields of a row of the parsed event csv
out: sxml data for the event
|#
(define (gen-event-sxml/bilingual event)
  (let* ([print-summary-french-accessor (get-field (hash-ref (fields-names) "print-summary-french"))]
         [print-summary-english-accessor (get-field (hash-ref (fields-names) "print-summary-english"))]
         [event-name-accessor (get-field (hash-ref (fields-names) "event-name"))]
         [location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
         [start-time-accessor (get-field (hash-ref (fields-names) "start-time"))]
         [start-date-accessor (get-field (hash-ref (fields-names) "start-date"))]
         [price-range-accessor (get-field (hash-ref (fields-names) "price-range"))]
         [phone-number-accessor (get-field (hash-ref (fields-names) "phone-number"))]
         [event-region-accessor (get-field (hash-ref (fields-names) "event-region"))]
         [period (lambda (string) (and string (string-append string ". ")))]
         [price-range-default (or (period (hash-ref (defaults) "price-range")) "")]
         [phone-number-default (or (period (hash-ref (defaults) "phone-number")) "")]
         [event-region (event-region-accessor event)]
         [print-summary (period (or (print-summary-french-accessor event);hope for a french summary
                                     (print-summary-english-accessor event);otherwise, use english summary
                                     (event-name-accessor event)))];if neither french nor english summary, use event name
         [location-abbreviation (period (location-abbreviation-accessor event))]
         [start-time (period (~t (start-time-accessor event) "H'h'mm"))]
         [price-range (or (period (price-range-accessor event)) price-range-default)]
         [phone-number (or (period (phone-number-accessor event)) phone-number-default)])
    `(li (@ (class "event-item"))
         (p ,(if (string=? "Greater Montreal Area" event-region)
                 `(span (@ (class "item-marker")) ">")
                 `(span (@ (class "item-marker")) ,(format "~a" (->day (start-date-accessor event)))))
            (span (@ (class "event-info"))
                  (span (@ (class "tab-character")) "???TAB???")
                  (span (@ (class "event-time")) ,start-time)
                  (span (@ (class "location-id")) ,location-abbreviation)
                  (span (@ (class "event-price")) ,price-range)
                  (span (@ (class "event-summary")) ,print-summary)
                  (span (@ (class "event-phone-number")) ,phone-number))))
    )
)

(define (gen-region-sxml/bilingual region-en region-fr locations-listing-sxml months-sxml)
  `(div (@ (class "region"))
        (h1 ,region-fr)
        ,locations-listing-sxml
        ,@months-sxml
        ))

(define (gen-locations-listing-sxml/bilingual locations)
  (let ([location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
        [location-name-accessor (get-field (hash-ref (fields-names) "location-name"))]
        [location-address-accessor (get-field (hash-ref (fields-names) "location-address"))]
        [location-city-accessor (get-field (hash-ref (fields-names) "location-city"))])
    `(div (@ (class "locations-listing"))
          ,@(for/list ([location locations])
              (let ([location-abbreviation (location-abbreviation-accessor location)]
                    [location-name (location-name-accessor location)]
                    [location-address (location-address-accessor location)]
                    [location-city (location-city-accessor location)])
                `(p (@ (class "location-info")) (b ,location-abbreviation) " " ,location-name ", " ,location-address ", " ,location-city))
              )
          )))

#|
sxml generator for a month in the bilingual calendar
in: month-en : string : name of the month in english
in: month-fr : string : name of the month in french
in: days : list : list of sxml data for each days in the month
out:  resulting sxml for the month
|#
(define (gen-month-sxml/english month-en days)
  `(div (@ (class "month"))
        (h2 ,(string-upcase month-en))
        ,@days))

#|
Template for a day in the english calendar
|#
(define (gen-day-sxml/english day-num day-en events-sxml)
  `(div (@ (class "day"))
        (h3 ,(string-titlecase day-en) " " ,(number->string day-num))
        (ul (@ (class "events-list")) ,@events-sxml)))

#|
template for an event in the english calendar
|#
(define (gen-event-sxml/english event)
  (let* ([print-summary-french-accessor (get-field (hash-ref (fields-names) "print-summary-french"))]
         [print-summary-english-accessor (get-field (hash-ref (fields-names) "print-summary-english"))]
         [event-name-accessor (get-field (hash-ref (fields-names) "event-name"))]
         [location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
         [start-time-accessor (get-field (hash-ref (fields-names) "start-time"))]
         [start-date-accessor (get-field (hash-ref (fields-names) "start-date"))]
         [price-range-accessor (get-field (hash-ref (fields-names) "price-range"))]
         [phone-number-accessor (get-field (hash-ref (fields-names) "phone-number"))]
         [event-region-accessor (get-field (hash-ref (fields-names) "event-region"))]
         [period (lambda (string) (and string (string-append string ". ")))]
         [price-range-default (or (period (hash-ref (defaults) "price-range")) "")]
         [phone-number-default (or (period (hash-ref (defaults) "phone-number")) "")]
         [event-region (event-region-accessor event)]
         [print-summary (period (or (print-summary-english-accessor event);first, try hope for an english summary
                                    (print-summary-french-accessor event);otherwise, fall back on the french one
                                    (event-name-accessor event)))];if neither exist, use event name
         [location-abbreviation (period (location-abbreviation-accessor event)) ]
         [start-time (period (~t (start-time-accessor event) "h:mma" #:locale "en"))]
         [price-range (period (or (price-range-accessor event) price-range-default))]
         [phone-number (period (or (phone-number-accessor event) phone-number-default))])
    `(li (@ (class "event-item"))
         (p ,(if (string=? "Greater Montreal Area" event-region)
                 `(span (@ (class "item-marker")) ">")
                 `(span (@ (class "item-marker")) ,(format "~a" (->day (start-date-accessor event)))))
            (span (@ (class "event-info"))
                  (span (@ (class "tab-character")) "???TAB???")
                  (span (@ (class "event-time")) ,start-time)
                  (span (@ (class "location-id")) ,location-abbreviation)
                  (span (@ (class "event-price")) ,price-range)
                  (span (@ (class "event-summary")) ,print-summary)
                  (span (@ (class "event-phone-number")) ,phone-number))))
    )
)

(define (gen-region-sxml/english region-en locations-listing-sxml months-sxml)
  `(div (@ (class "region"))
        (h1 ,(string-upcase region-en))
        ,locations-listing-sxml
        ,@months-sxml
        ))

(define (gen-locations-listing-sxml/english locations)
  (let ([location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
        [location-name-accessor (get-field (hash-ref (fields-names) "location-name"))]
        [location-address-accessor (get-field (hash-ref (fields-names) "location-address"))]
        [location-city-accessor (get-field (hash-ref (fields-names) "location-city"))])
    `(div (@ (class "locations-listing"))
          ,@(for/list ([location locations])
              (let ([location-abbreviation (location-abbreviation-accessor location)]
                    [location-name (location-name-accessor location)]
                    [location-address (location-address-accessor location)]
                    [location-city (location-city-accessor location)])
                `(p (@ (class "location-info")) (b ,location-abbreviation) " " ,location-name ", " ,location-address ", " ,location-city))
              )
          )))

#|
sxml generator for a month in the bilingual calendar
in: month-en : string : name of the month in english
in: month-fr : string : name of the month in french
in: days : list : list of sxml data for each days in the month
out:  resulting sxml for the month
|#
(define (gen-month-sxml/french month-fr days-sxml)
  `(div (@ (class "month"))
        (h2 ,(string-upcase month-fr))
        ,@days-sxml))

#|
Template for a day in the bilingual calendar
|#
(define (gen-day-sxml/french day-fr day-num events-sxml)
  `(div (@ (class "day"))
        (h3 ,(string-downcase day-fr) " " ,(number->string day-num))
        (ul (@ (class "events-list")) ,@events-sxml)))

(define (period string)
  (and string (string-append string ".")))

#|
template for an event in the bilingual calendar
|#
(define (gen-event-sxml/french event)
  (let* ([print-summary-french-accessor (get-field (hash-ref (fields-names) "print-summary-french"))]
         [print-summary-english-accessor (get-field (hash-ref (fields-names) "print-summary-english"))]
         [event-name-accessor (get-field (hash-ref (fields-names) "event-name"))]
         [location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
         [start-time-accessor get-start-time]
         [start-date-accessor get-start-date]
         [price-range-accessor (get-field (hash-ref (fields-names) "price-range"))]
         [phone-number-accessor (get-field (hash-ref (fields-names) "phone-number"))]
         [event-region-accessor (get-field (hash-ref (fields-names) "event-region"))]
         [price-range-default (or (period (hash-ref (defaults) "price-range")) "")]
         [phone-number-default (or (period (hash-ref (defaults) "phone-number")) "")]
         [event-region (event-region-accessor event)]
         [print-summary (period (or (print-summary-french-accessor event)
                                    (print-summary-english-accessor event);if no french summary, uses english summary
                                    (event-name-accessor event)))];if neither french nor english summary, uses title of event
         [location-abbreviation (period (location-abbreviation-accessor event))]
         [start-time (period (~t (start-time-accessor event) "H'h'mm"))]
         [price-range (period (or (price-range-accessor event) price-range-default))]
         [phone-number (period (or (phone-number-accessor event) phone-number-default))])
    `(li (@ (class "event-item"))
         (p ,(if (string=? "Greater Montreal Area" event-region)
                 `(span (@ (class "item-marker")) ">")
                 `(span (@ (class "item-marker")) ,(format "~a" (->day (start-date-accessor event)))))
            (span (@ (class "event-info"))
                  (span (@ (class "tab-character")) "???TAB???")
                  (span (@ (class "event-time")) ,start-time)
                  (span (@ (class "location-id")) ,location-abbreviation)
                  (span (@ (class "event-price")) ,price-range)
                  (span (@ (class "event-summary")) ,print-summary)
                  (span (@ (class "event-phone-number")) ,phone-number))))
    )
)

(define (gen-region-sxml/french region-fr locations-listing-sxml months-sxml)
  `(div (@ (class "region"))
        (h1 ,(string-upcase region-fr))
        ,locations-listing-sxml
        ,@months-sxml
        ))

(define (gen-locations-listing-sxml/french locations)
  (let ([location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))]
        [location-name-accessor (get-field (hash-ref (fields-names) "location-name"))]
        [location-address-accessor (get-field (hash-ref (fields-names) "location-address"))]
        [location-city-accessor (get-field (hash-ref (fields-names) "location-city"))])
    `(div (@ (class "locations-listing"))
          ,@(for/list ([location locations])
              (let ([location-abbreviation (location-abbreviation-accessor location)]
                    [location-name (location-name-accessor location)]
                    [location-address (location-address-accessor location)]
                    [location-city (location-city-accessor location)])
                `(p (@ (class "location-info")) (b ,location-abbreviation) " " ,location-name ", " ,location-address ", " ,location-city))
              )
          )))




#|
returns a getter function for 'field'
in: field: string: (should be a string which is the name of a column of the row)
out: getter function which takes a row (association list) as input and returns the value of the field as output
|#
(define (get-field field)
  (lambda (row)
    (match (assoc field row)
      [(cons _ value) value]
      [#f (error (format "Missing field ~a in row ~a" field row))])))

(define get-start-date (get-field (hash-ref (fields-names) "start-date")))
(define get-start-time (get-field (hash-ref (fields-names) "start-time")))


#|
This returns a hash table mapping from a month(number) to a list of events occuring in that month
in: events: list of list: list of events(which are association lists of field name/field value)
out: hash table: a mapping from month number to list of events
|#
(define (group-by-month events)
  (for/hash ([group (group-by get-month events =)])
    (values (get-month (car group)) group)))

#|
This returns a hash table mapping from a day(number from 1 to 31) to a list of events occuring on that day
in: events: list of list: list of events(which are association lists of field name/field value)
out: hash table: a mapping from day number to list of events
|#
(define (group-by-day events)
  (for/hash ([group (group-by get-day events =)])
    (values (get-day (car group)) group)))


#|
This returns a hash table mapping from a date(see gregor library) to a list of events occuring on that date
in: events: list of list: list of events(which are association lists of field name/field value)
out: hash table: a mapping from date to list of events
|#
(define (group-by-date events)
  (for/hash ([group (group-by get-date events date=?)])
    (values (get-date (car group)) group)))

(define (translate-region/fr region-en)
  (match region-en
    ["Quebec (elsewhere)" "Québec (ailleurs)"]
    ["Greater Montreal Area" "Région de Montréal"]
    ["Greater Quebec city Area" "Région de la ville de Québec"]
    ["Ottawa/Gatineau region" "Région d'Ottawa/Gatineau"]
    ["Ontario (elsewhere)" "Ontario (ailleurs)"]
    [_ region-en]))



#|
Transforms the s-expression string representing an association list, into an actual association list.
Also transforms the symbols used in the association list to strings
in: mapping: a string representing an association list of symbols to symbols
out: an association list of string to string
|#
(define (parse-mapping-string mapping)
  (let ([form (read (open-input-string mapping))])
    (if (list? form)
        (for/list ([pair form])
          (match pair
            [(cons (? symbol? field) (? symbol? fieldname)) (cons (symbol->string field) (symbol->string fieldname))]
            [_ (error (format "Bad syntax for association list: ~a. \n See documentation for help." mapping))]))
        (error (format "Bad syntax for association list: ~a. \n See documentation for help." mapping)))))

#|
Parses a string containing mappings for new field names(in the form of an association list),
and set each field name to the new parsed value
in: field-names-string: a string representing an association list of symbols to symbols
out: nothing useful (void)
|#

(define (set-fields-names field-names-string)
  (let ([field-names-assoc (parse-mapping-string field-names-string)])
    (for ([pair field-names-assoc])
      (match pair
        [(cons field field-name) (hash-set! (fields-names) field field-name)]))))

(define (set-default-values defaults-string)
  (let ([defaults-assoc (parse-mapping-string defaults-string)])
    (for ([pair defaults-assoc])
      (match pair
        [(cons param value) (hash-set! (defaults) param value)]))))


(define (print-defaults)
  (for ([(key value) (defaults)])
    (displayln (format "~a : ~a" key value)))
  (newline)
  (displayln "Fields names: ")
  (newline)
  (for ([(key value) (fields-names)])
    (displayln (format "~a : '~a'" key value))))

#|
Setting up a logger for debug info
See https://docs.racket-lang.org/reference/logging.html
|#

;;the logger
(define logger (make-logger 'program-info))

;;the log receiver
(define debug (make-log-receiver logger 'debug))

(current-logger logger)

(define log-output-port (make-parameter (open-output-string "default log output port")))

(define debug-mode (make-parameter #f))

#|
Convert csv data read from an input file to html
|#
(define (csv->html events-csv-file-path locations-csv-file-path [html-file-path #f])
  (let* ([events-csv-input-port (open-input-file events-csv-file-path)]
         [output-port (if html-file-path (open-output-file html-file-path #:exists 'replace) (html-output-port))]
         [locations-csv-input-port (open-input-file locations-csv-file-path)]
         [events-reader (make-csv-reader events-csv-input-port reader-spec)];the reader used to parse the csv file
         [locations-reader (make-csv-reader locations-csv-input-port reader-spec)];the reader used to parse the csv file
         ;it is assumed that the first row of a csv contains the columns' name. This row is used to annotate the fields of each events/location
         [events-columns (events-reader)]
         [locations-columns (locations-reader)]
         ;a list of events, which are association lists (fieldName . fieldValue)
         [events (map (compose parse-row (curry map cons events-columns)) (csv->list events-reader))]
         [events-locations (map (get-field (hash-ref (fields-names) "event-location-id")) events)]
         ;a list of locations, which are association lists (fieldName . fieldValue)
         ;Only locations linked to an event are retained
         [locations (filter (lambda (loc) (member ((get-field (hash-ref (fields-names) "location-id")) loc) events-locations))
                            (map (curry map cons locations-columns) (csv->list locations-reader)))]
         [get-location-region (get-field (hash-ref (fields-names) "location-region"))]
         [get-event-region (get-field (hash-ref (fields-names) "event-region"))]
         [document-title (case (calendar-edition)
                           [("bilingual") "Bilingual Print Calendar"]
                           [("english") "English Print Calendar"]
                           [("french") "French Print Calendar"])]
         )

    ;;Running a thread to capture and print logs to log-output-port
    (void
     (thread
      (lambda ()
        (let loop ()
          (let ([v (sync debug)])
            (fprintf (log-output-port) "[~a] ~a\n" (vector-ref v 0) (vector-ref v 1))
            (flush-output (log-output-port))
            (loop))))))

    (log-debug "Event Fields: ~s" events-columns)
    (log-debug "Location Fields: ~s" locations-columns)
    (log-debug "Exemple Event: ~s" (car events))
    (log-debug "Exemple Location: ~s" (car locations))

    ;;a hash table (region => locations in this region)
    (define locations-by-region
      (for/hash ([group (group-by get-location-region locations string=?)])
        (let ([location-abbreviation-accessor (get-field (hash-ref (fields-names) "location-abbreviation"))])
          (values (get-location-region (car group)) (sort group string<? #:key location-abbreviation-accessor)))))

    ;;a hash table (region => events in this region)
    (define events-by-region
      (for/hash ([group (group-by get-event-region events string=?)])
        (values (get-event-region (car group)) group)))

    ;(pretty-print events-by-region)
    (log-debug "Regions for locations-by-region table: ~s" (hash-keys locations-by-region))
    (log-debug "Regions for events-by-region table: ~s" (hash-keys events-by-region))

    ;;a list of sxml for each region
    (define regions-sxml
      (for/list ([(region events) events-by-region])
        (let ([region-en region]
              [region-fr (translate-region/fr region)])

          (log-debug "In region ~s" region)
          (log-debug "Number of events: ~s" (length events))

          ;;a hash table (date => events on this date)
          (define events-by-date
            (for/hash ([group (group-by get-date events date=?)])
              (values (get-date (car group)) group)))



          ;;sxml for the region's location listing
          (define locations-listing-sxml
            (case (calendar-edition)
              [("bilingual") (gen-locations-listing-sxml/bilingual
                              (hash-ref locations-by-region region
                                        (lambda ()
                                          (error (format "No locations found in region ~a.)" region)))))]
              [("english") (gen-locations-listing-sxml/english
                              (hash-ref locations-by-region region
                                        (lambda ()
                                          (error (format "No locations found in region ~a.)" region)))))]
              [("french") (gen-locations-listing-sxml/french
                              (hash-ref locations-by-region region
                                        (lambda ()
                                          (error (format "No locations found in region ~a.)" region)))))]))

          ;;a hash table (date => sxml data for the date)
          (define sxml-by-date
            (for/hash ([(key events) events-by-date]);key is the date
              (let* ([day (->day key)]
                     [day-en (~t key "EEEE" #:locale "en")];name of the day(monday-sunday) in english
                     [day-fr (~t key "EEEE" #:locale "fr")]
                     [sorted-events (sort events time<? #:key get-start-time)])
                (values key
                        (if (string=? region "Greater Montreal Area")
                            (case (calendar-edition)
                              [("bilingual") (gen-day-sxml/bilingual
                                              day-fr day day-en
                                              (map gen-event-sxml/bilingual sorted-events))]
                              [("english") (gen-day-sxml/english
                                            day day-en
                                            (map gen-event-sxml/english sorted-events))]
                              [("french") (gen-day-sxml/french
                                           day-fr day
                                           (map gen-event-sxml/french sorted-events))])

                            (case (calendar-edition)
                              [("bilingual") `(div (@ (class "day"))
                                                   (ul (@ (class "events-list"))
                                                       ,@(map gen-event-sxml/bilingual sorted-events)))]

                              [("english") `(div (@ (class "day"))
                                                 (ul (@ (class "events-list"))
                                                     ,@(map gen-event-sxml/english
                                                            sorted-events)))]
                              [("french") `(div (@ (class "day"))
                                                (ul (@ (class "events-list"))
                                                    ,@(map gen-event-sxml/french
                                                           sorted-events)))])
                            )
                        ))))

          ;;a list of sxml data for each month in order of months(ascending)
          (define months-sxml
            (for/list ([dates-by-month (sort (group-by ->month (hash-keys sxml-by-date) =) date<? #:key car)]);list of dates grouped by month, sorted
              (let* ([month (->month (car dates-by-month))]
                     [month-en (month->string/en month)]
                     [month-fr (month->string/fr month)]
                     [sorted-dates (sort dates-by-month date<? )]
                     )

                (log-debug "In Month: ~s (~s) (~s)" month month-en month-fr)

                (case (calendar-edition)
                  [("bilingual") (gen-month-sxml/bilingual
                                  month-en month-fr
                                  (map (lambda (event-date) (hash-ref sxml-by-date event-date)) sorted-dates))]
                  [("english") (gen-month-sxml/english
                                month-en
                                (map (lambda (event-date) (hash-ref sxml-by-date event-date)) sorted-dates))]
                  [("french") (gen-month-sxml/french
                                  month-fr
                                  (map (lambda (event-date) (hash-ref sxml-by-date event-date)) sorted-dates))]))))

          (case (calendar-edition)
            [("bilingual") (gen-region-sxml/bilingual region-en region-fr locations-listing-sxml months-sxml)]
            [("english") (gen-region-sxml/english region-en locations-listing-sxml months-sxml)]
            [("french") (gen-region-sxml/french region-fr locations-listing-sxml months-sxml)]))))

    (let* ([doc-title (case (calendar-edition)
                        [("bilingual") "Bilingual Calendar Listing"]
                        [("english") "English Calendar Listing"]
                        [("french") "French Calendar Listing"])]
           [document (gen-document-sxml regions-sxml doc-title)])
      (log-debug "debug-mode? ~a" (debug-mode))
      (if (debug-mode) (display (get-output-string (log-output-port)) (current-output-port)) (void));if debug-mode is true, printing logs to stdout
      (write-html document output-port))
    ))

(define main
  (command-line
   #:program "Print calendar html generator"
   #:once-any
   [("--bi" "--bilingual") "Generate html for the bilingual edition" (calendar-edition "bilingual")]
   [("--en" "--english") "Generate html for the english edition" (calendar-edition "english")]
   [("--fr" "--french") "Generate html for the french edition" (calendar-edition "french")]
   #:once-each
   [("-v" "--verbose") ("Enable verbose mode, which print debugging information to standard output."
                        "Warning: if this is enabled and a file is not specified for the html output,"
                        "both debug information and the html will be outputted to standard output, "
                        "and piping/redirecting the output of the program will also pipe/redirect the debug information.")
    (debug-mode #t)]
   [("-f" "--fields-name") field-name-string
    ("Name of the csv fields that are used to generate the html." "The format is \"((field . |fieldname|) ...)\". See doc for details."
     "'field' can be one of: "
     "    'start-date': start date of the event,"
     "    'start-time': start time of the event,"
     "    'location-name': name of the location, "
     "    'phone-number': phone number given by the event's organizer"
     "    'print-summary-french': french print summary of the event,"
     "    'print-summary-english': english print summary of the event, "
     "    'price-range': price range of the event, "
     "    'event-region': region of the event, "
     "    'location-region': region of the location, "
     "    'location-address': address of the location, "
     "    'event-abbreviation': abbreviation of the location associated to an event, "
     "    'location-abbreviation': abbreviation of a location, "
     "    'event-name': name of the event, "
     "    'event-location-id': id of the location associated to an event, "
     "    'location-id': id of a location."
     "Default field names are:"
     "    start-date: \"_event_start_date\""
     "    start-time: \"_event_start_time\""
     "    location-name: \"Title\""
     "    event-name: \"Title\""
     "    event-region: \"_location_region\""
     "    location-region: \"_location_region\""
     "    print-summary-english: \"print-summary-english\""
     "    print-summary-french: \"print-summary-french\""
     "    phone-number: \"event-phone-number\""
     "    price-range: \"price-range\""
     "    location-city: \"_location_town\""
     "    location-abbreviation: \"abbreviation\""
     "    event-abbreviation: \"abbreviation\""
     "    location-address: \"_location_address\""
     "    location-id: \"_location_id\""
     "    event-location-id: \"_location_id\""

     )
    (set-fields-names field-name-string)]
   [("-d" "--date-format") date-format-string
    ("String representing the format of the date."
     "See http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table for details."
     "The default date format string is \"M/d/yyyy\"") (hash-set! (defaults) "date-format" date-format-string)]
   [("-t" "--time-format") time-format-string
    ("String representing the format of the time of day."
     "See http://unicode.org/reports/tr35/tr35-dates.html#Date_Field_Symbol_Table for details."
     "The default time format string is \"H:mm:ss\"") (hash-set! (defaults) "time-format" time-format-string)]
   [("--defaults") default-values-string ("String specifying the default values for some fields."
                                          "The format is \"field:default value\"."
                                          "Fields which have default values are: 'price-range', 'phone-number'."
                                          "By default, those default values for those fields are:"
                                          "   price-range: \"Free/Gratuit\""
                                          "   phone-number: \"N.A.\"")
    (displayln default-values-string)
    ]
   #:final
   [("--print-parameters") "Print the default values of the program's parameters."
    (print-defaults)]
   [("-o" "--output") html-output-path ("Specifies the path of the file in which the generated html will be outputted"
                                        "If this flag is not used, the html will be outputted on the standard output")
    (html-output-port (open-output-file html-output-path #:exists 'replace))]
   #:args (events-csv-path locations-csv-path) (csv->html events-csv-path locations-csv-path)

   ))
