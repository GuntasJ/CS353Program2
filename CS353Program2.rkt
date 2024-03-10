#lang racket
;helper function to make getting user input easier


(define (read-line-input)
  (read-line (current-input-port) 'any))

(define (get-index formatted-data-line)
  (first formatted-data-line))

(define (get-rank formatted-data-line)
  (second formatted-data-line))

(define (get-title formatted-data-line)
  (third formatted-data-line))

(define (get-platform formatted-data-line)
  (fourth formatted-data-line))

(define (get-date formatted-data-line)
  (fifth formatted-data-line))

(define (get-genre formatted-data-line)
  (sixth formatted-data-line))

(define (get-publisher formatted-data-line)
  (seventh formatted-data-line))

(define (get-north-america formatted-data-line)
  (eighth formatted-data-line))

(define (get-europe formatted-data-line)
  (ninth formatted-data-line))

(define (get-japan formatted-data-line)
  (tenth formatted-data-line))

(define (get-rotw formatted-data-line)
  (list-ref formatted-data-line 10))

(define (get-global formatted-data-line)
  (list-ref formatted-data-line 11))

(define (get-review formatted-data-line)
  (if (= (length formatted-data-line) 13)
      (list-ref formatted-data-line 12)
      (ninth formatted-data-line)))




(define (file-data->formatted-data-set file-data)
  (define (file-line->formatted-line line)
    (define (fix-split-line pre-split-line)
      (append (take pre-split-line 2) (list (string-append (third pre-split-line) "," (fourth pre-split-line))) (drop pre-split-line 4)))  

    (define pre-split-line (string-split line ","))
    (define split-line (if (= (length pre-split-line) 14)
                           (fix-split-line pre-split-line)
                           pre-split-line))
    (define (remove-quotes entry) (if (non-empty-string? entry)
                                      (substring entry 1 (sub1 (string-length entry)))
                                      entry))
    (list
     ;index
     (string->number (first split-line))
     ;rank
     (string->number (second split-line))
     ;Game Title
     (remove-quotes (third split-line))
     ;Platform
     (string->symbol (remove-quotes (fourth split-line)))
     ;Year
     (string->number (fifth split-line))
     ;Genre
     (string->symbol (remove-quotes (sixth split-line)))
     ;Publisher
     (remove-quotes (seventh split-line))
     ;North America
     (string->number (eighth split-line))
     ;Europe
     (string->number (ninth split-line))
     ;Japan
     (string->number (tenth split-line))
     ;Rest of World
     (string->number (list-ref split-line 10))
     ;Global
     (string->number (list-ref split-line 11))
     ;Review
     (string->number (list-ref split-line 12))))
  
  (map file-line->formatted-line file-data))

;(file-data->formatted-data-set (rest (file->lines "Video Games Sales.csv")))


(define (print-sort-prompt)
  (displayln "Do you want to sort the fetched results by sales or by rating?"))

(define (print-name-prompt)
  (displayln "Enter the name of the game. Note that the manager accepts case-insensitive input, however, the spelling must be exact."))

(define (print-preface)
  (displayln "Welcome to the Dataset Manager Program")
  (displayln "With the data, you can filter or sort the dataset")
  (displayln "")
  (displayln "Enter quit to exit out of the program")
  (displayln ""))

(define (print-options)
  (displayln "Select up to three of the following options. Enter q to break early and select less than 3 of the options: ")
  (displayln "Name")
  (displayln "Region")
  (displayln "Date")
  (displayln "Publisher")
  (displayln "Genre"))


(define (get-name-input)
  (displayln "Enter the name of the game. Spelling counts, however, the search will be case-insensitive")
  (define name-input (read-line-input))
  (list 'NAME name-input))

(define (get-date-input)
  (displayln "Enter the start year to filter: ")
  (define start-date (read-line-input))
  (displayln "Enter the end year to filter: ")
  (define end-date (read-line-input))
  (cond
    [(and (andmap char-numeric? (string->list start-date)) (andmap char-numeric? (string->list end-date)))
     (if (< (string->number start-date) (string->number end-date))
         (list 'DATE (string->number start-date) (string->number end-date))
         (list 'DATE (string->number end-date) (string->number start-date)))]
    [else
     (displayln "One of your dates is formatted incorrectly. Enter again")
     (get-date-input)]))

(define (get-publisher-input)
  (displayln "Enter the publisher. This is case-insensitive and it accepts a partial match")
  (define publisher-input (read-line-input))
  (list 'PUBLISHER publisher-input))

(define (get-region-input)
  (displayln "Enter the region. Input must be one of the following: North America, Europe, Japan, Rest of the World, Global")
  (define region-input (read-line-input))
  (cond
    [(string-ci=? region-input "North America")     (list 'Z_REGION 'NORTH-AMERICA)]
    [(string-ci=? region-input "Europe")            (list 'Z_REGION 'EUROPE)]
    [(string-ci=? region-input "Japan")             (list 'Z_REGION 'JAPAN)]
    [(string-ci=? region-input "Rest of the World") (list 'Z_REGION 'ROTW)]
    [(string-ci=? region-input "Global")            (list 'Z_REGION 'GLOBAL)]
    [else
     (displayln "Invalid region. Please try again: ")
     (get-region-input)]))

(define (get-genre-input)
  (displayln "Enter the genre")
  (define genre-input (read-line-input))
  (list 'GENRE (string->symbol genre-input)))

(define (run-menu)
  (define formatted-data-set (file-data->formatted-data-set (rest (file->lines "Video Games Sales.csv"))))
  (print-preface)
  
  (define (get-user-selections [selections '()])

    (define (contains-selection? choice selections)
      (define choice-symbol (cond
                              [(string-ci=? choice "Name") 'NAME]
                              [(string-ci=? choice "Date") 'DATE]
                              [(string-ci=? choice "Publisher") 'PUBLISHER]
                              [(string-ci=? choice "Region") 'Z_REGION]
                              [(string-ci=? choice "Genre") 'GENRE]
                              [else 'ERROR]))
        (> (length (filter (λ (element) (if (symbol? element)
                                            (symbol=? element choice-symbol)
                                            #f))
                                            (flatten selections))) 0))
    
    (cond
      [(equal? (length selections) 3) selections]
      [else
       (print-options)
       (define user-criteria-choice (read-line-input))
       (cond
         [(contains-selection? user-criteria-choice selections)
          (displayln "Cannot enter a selection you have already entered. Please try again")
          (get-user-selections selections)]
         [(string-ci=? user-criteria-choice "q") selections]
         [(string-ci=? user-criteria-choice "Name")      (get-user-selections (cons (get-name-input) selections))]
         [(string-ci=? user-criteria-choice "Date")      (get-user-selections (cons (get-date-input) selections))]
         [(string-ci=? user-criteria-choice "Publisher") (get-user-selections (cons (get-publisher-input) selections))]
         [(string-ci=? user-criteria-choice "Region")    (get-user-selections (cons (get-region-input) selections))]
         [(string-ci=? user-criteria-choice "Genre")     (get-user-selections (cons (get-genre-input) selections))]
         [(string-ci=? user-criteria-choice "quit")      (list 'QUIT)]
         [else
          (displayln "Invalid selection. Please try again")
          (get-user-selections selections)])]))

  (define selections (sort (get-user-selections) symbol<? #:key first))
  
  (define (process-selection selection data-set)
    (define (process-name name-selection data-set)
      (filter (λ (formatted-line) (string-ci=? (get-title formatted-line) (first name-selection))) data-set))

    (define (process-date date-selection data-set)
      (filter (λ (formatted-line) (and (>= (get-date formatted-line) (first date-selection)) (< (get-date formatted-line) (second date-selection)))) data-set))
    
    (define (process-publisher publisher-selection data-set)
      (filter (λ (formatted-line) (if (equal? (memf (λ (arg) (if (string? arg)
                                                                 (string-ci=? arg (first publisher-selection))
                                                                 (equal? arg (first publisher-selection)))) formatted-line) #f)
                                      #f
                                      #t)) data-set))
                                                                              
    
    (define (process-region region-selection data-set)
      (cond
        [(equal? (first region-selection) 'NORTH-AMERICA)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-north-america fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'EUROPE)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-europe fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'JAPAN)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-japan fl) (get-review fl))) data-set)]
        [(equal? (first region-selection) 'ROTW)
         (map (λ (fl) (list (get-index fl) (get-rank fl) (get-title fl) (get-platform fl) (get-date fl) (get-genre fl) (get-publisher fl) (get-rotw fl) (get-review fl))) data-set)]
        [else data-set]))    
    
    (define (process-genre genre-selection data-set)
      (filter (λ (formatted-line) (equal? (first genre-selection) (get-genre formatted-line))) data-set))

    (cond
      [(equal? 'NAME (first selection))      (process-name (rest selection) data-set)]
      [(equal? 'DATE (first selection))      (process-date (rest selection) data-set)]
      [(equal? 'PUBLISHER (first selection)) (process-publisher (rest selection) data-set)]
      [(equal? 'Z_REGION (first selection))  (process-region (rest selection) data-set)]
      [(equal? 'GENRE (first selection))     (process-genre (rest selection) data-set)]))

  (define (process-selections selections data-set)
    (if (empty? selections)
        data-set
        (process-selections (rest selections) (process-selection (first selections) data-set))))

  (cond
    [(equal? selections (list 'QUIT))
     (displayln "Exited the program")
     'QUIT]
    [else
     (print-sort-prompt)
     (define (get-sort-function)
       (define user-sort-input (read-line-input))
       (cond
         [(string-ci=? user-sort-input "Sales")  (λ (data-set) (sort data-set < #:key get-rank))]
         [(string-ci=? user-sort-input "Rating") (λ (data-set) (sort data-set < #:key get-review))]
         [else
          (displayln "Invalid choice. Choice must be sales or rating. Please enter again")
          (get-sort-function)]))
     (define user-sort (get-sort-function))
     (displayln "Index  Rank  Title  Platform  Year  Genre  Publisher  NA  E  J  ROTW  Global  Review")
     (for-each displayln (user-sort (process-selections selections formatted-data-set)))
     (displayln "")]))


(define (while-not-q)
  (when (not (equal? (run-menu) 'QUIT))
    (while-not-q)))



;(run-menu)
(while-not-q)
     
