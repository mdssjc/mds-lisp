;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname |12-Projects: Lists|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 12-Projects: Lists.rkt
;; II - Arbitrarily Large Data
;; 12 - Projects: Lists

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)
(require 2htdp/itunes)


;; 12.1 - Real-World Data: Dictionaries

;; Exercise 195
;; Exercise 196
;; Exercise 197
;; Exercise 198


;; =================
;; Constants:

; On OS X: /usr/share/dict/words
; On LINUX: /usr/share/dict/words or /var/lib/dict/words
; On WINDOWS: borrow the word file from your Linux friend
(define LOCATION "/usr/share/dict/words")


;; =================
;; Data definitions:

; A Letter is one of the following 1Strings:
; - "a"
; - ...
; - "z"
; or, equivalently, a member? of this list:
(define LETTERS (explode "abcdefghijklmnopqrstuvwxyz"))

; A Dictionary is a List-of-strings.
(define AS-LIST (read-lines LOCATION))
(define DICTIONARY-EMPTY  '())
(define DICTIONARY-AS-LIST (list "alfa" "eco" "bravo" "erlang" "charlie" "zulu"))

; A LoL is one of:
; '()
; (cons Letter LoL)
; interpretation the list of Letters is a collection of Letter

; A Letter-Count is one of:
; '()
; '(list Letter Number)
; interpretation the Letter-Count is a piece of data that combines letter and count

; A LoLC is one of:
; '()
; (cons Letter-Count LoLC)
; interpretation the list of Letter-Counts is a collection of Letter-Count

; A LoD is one of:
; - '()
; - (cons String LoD)
; interpretation the list of Dictionary is a collection of Dictionary


;; =================
;; Functions:

; Dictionary Letter -> Number
; counts how many words in the given Dictionary d start wn Letter l
(check-expect (starts-with# DICTIONARY-EMPTY   "e") 0)
(check-expect (starts-with# DICTIONARY-AS-LIST "e") 2)
(check-expect (starts-with# DICTIONARY-AS-LIST "z") 1)

(define (starts-with# d l)
  (cond [(empty? d) 0]
        [else (+ (if (string=? (string-ith (first d) 0) l) 1 0)
                 (starts-with# (rest d) l))]))


(starts-with# AS-LIST "e")              ; 4006
(starts-with# AS-LIST "z")              ; 190

; Dictionary -> LoLC
; counts how often each letter is used as the first one of a word in the given dictionary
(check-expect (count-by-letter DICTIONARY-EMPTY)  '())
(check-expect (count-by-letter DICTIONARY-AS-LIST) (list
                                                    (list "a" 1)
                                                    (list "b" 1)
                                                    (list "c" 1)
                                                    (list "d" 0)
                                                    (list "e" 2)
                                                    (list "f" 0)
                                                    (list "g" 0)
                                                    (list "h" 0)
                                                    (list "i" 0)
                                                    (list "j" 0)
                                                    (list "k" 0)
                                                    (list "l" 0)
                                                    (list "m" 0)
                                                    (list "n" 0)
                                                    (list "o" 0)
                                                    (list "p" 0)
                                                    (list "q" 0)
                                                    (list "r" 0)
                                                    (list "s" 0)
                                                    (list "t" 0)
                                                    (list "u" 0)
                                                    (list "v" 0)
                                                    (list "w" 0)
                                                    (list "x" 0)
                                                    (list "y" 0)
                                                    (list "z" 1)))

(define (count-by-letter d)
  (cond [(empty? d) '()]
        (else (count-by-letters LETTERS d))))

; LoL Dictionary -> LoLC
; reports how often the given letters occur as first ones in the dictionary
(check-expect (count-by-letters LETTERS DICTIONARY-EMPTY) (list
                                                           (list "a" 0)
                                                           (list "b" 0)
                                                           (list "c" 0)
                                                           (list "d" 0)
                                                           (list "e" 0)
                                                           (list "f" 0)
                                                           (list "g" 0)
                                                           (list "h" 0)
                                                           (list "i" 0)
                                                           (list "j" 0)
                                                           (list "k" 0)
                                                           (list "l" 0)
                                                           (list "m" 0)
                                                           (list "n" 0)
                                                           (list "o" 0)
                                                           (list "p" 0)
                                                           (list "q" 0)
                                                           (list "r" 0)
                                                           (list "s" 0)
                                                           (list "t" 0)
                                                           (list "u" 0)
                                                           (list "v" 0)
                                                           (list "w" 0)
                                                           (list "x" 0)
                                                           (list "y" 0)
                                                           (list "z" 0)))
(check-expect (count-by-letters LETTERS DICTIONARY-AS-LIST) (list
                                                             (list "a" 1)
                                                             (list "b" 1)
                                                             (list "c" 1)
                                                             (list "d" 0)
                                                             (list "e" 2)
                                                             (list "f" 0)
                                                             (list "g" 0)
                                                             (list "h" 0)
                                                             (list "i" 0)
                                                             (list "j" 0)
                                                             (list "k" 0)
                                                             (list "l" 0)
                                                             (list "m" 0)
                                                             (list "n" 0)
                                                             (list "o" 0)
                                                             (list "p" 0)
                                                             (list "q" 0)
                                                             (list "r" 0)
                                                             (list "s" 0)
                                                             (list "t" 0)
                                                             (list "u" 0)
                                                             (list "v" 0)
                                                             (list "w" 0)
                                                             (list "x" 0)
                                                             (list "y" 0)
                                                             (list "z" 1)))

(define (count-by-letters lol d)
  (cond [(empty? lol) '()]
        [else (cons (list (first lol) (starts-with# d (first lol)))
                    (count-by-letters (rest lol) d))]))

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that is occurs most often
; as the first one in the given Dictionary
(check-expect (most-frequent DICTIONARY-EMPTY)  '())
(check-expect (most-frequent DICTIONARY-AS-LIST) (list "e" 2))

(define (most-frequent d)
  (maximum-count.v1 (count-by-letter d) '()))

; LoLC Letter-Count -> Letter-Count
; picks the pair with the maximum count
(check-expect (maximum-count.v1 (count-by-letter DICTIONARY-EMPTY)   '()) '())
(check-expect (maximum-count.v1 (count-by-letter DICTIONARY-AS-LIST) '())  (list "e" 2))
(check-expect (maximum-count.v2 (count-by-letter DICTIONARY-EMPTY)   '()) '())
(check-expect (maximum-count.v2 (count-by-letter DICTIONARY-AS-LIST) '())  (list "e" 2))

(define (maximum-count.v1 lolc max)
  (cond [(empty? lolc) max]
        [else
         (maximum-count.v1 (rest lolc)
                           (cond [(or (empty? max)
                                      (> (second (first lolc)) (second max)))
                                  (first lolc)]
                                 [else max]))]))

(define (maximum-count.v2 lolc max)
  (cond [(empty? lolc) max]
        [else (first (sort lolc))]))

; LoLC -> LoLC
; produces a sorted version of l
(check-expect (sort '()) '())
(check-expect (sort (list (list "a" 0) (list "b" 2) (list "c" 1)))
              (list (list "b" 2) (list "c" 1) (list "a" 0)))

(define (sort l)
  (cond [(empty? l) '()]
        [else (insert (first l)
                      (sort (rest l)))]))

; Letter-Count LoLC -> LoLC
; inserts x into the sorted list l
(check-expect (insert (list "a" 0) '())
              (list (list "a" 0)))
(check-expect (insert (list "a" 0) (list (list "b" 2)))
              (list (list "b" 2) (list "a" 0)))
(check-expect (insert (list "b" 2) (list (list "a" 0)))
              (list (list "b" 2) (list "a" 0)))
(check-expect (insert (list "c" 1) (list (list "b" 2) (list "a" 0)))
              (list (list "b" 2) (list "c" 1) (list "a" 0)))

(define (insert x l)
  (cond [(empty? l) (cons x '())]
        [else
         (cond [(>= (second x)
                    (second (first l)))
                (cons x l)]
               [else (cons (first l)
                           (insert x (rest l)))])]))

; Dictionary -> LoD
; produces a list of Dictionarys, one per Letter
(check-expect (words-by-first-letter DICTIONARY-EMPTY)  '())
(check-expect (words-by-first-letter DICTIONARY-AS-LIST) (list (list "alfa")
                                                               (list "bravo")
                                                               (list "charlie")
                                                               (list "eco" "erlang")
                                                               (list "zulu")))

(define (words-by-first-letter d)
  (words-by* LETTERS d))

; LoL Dictionary -> LoD
; produces a list of Dictionarys with all the words in the dictionary d for each letter lol
(check-expect (words-by* '()     DICTIONARY-AS-LIST) '())
(check-expect (words-by* '()     DICTIONARY-EMPTY)   '())
(check-expect (words-by* LETTERS DICTIONARY-EMPTY)   '())
(check-expect (words-by* LETTERS DICTIONARY-AS-LIST)  (list (list "alfa")
                                                            (list "bravo")
                                                            (list "charlie")
                                                            (list "eco" "erlang")
                                                            (list "zulu")))

(define (words-by* lol d)
  (cond [(empty? lol) '()]
        [else
         (cond [(empty? (words-by (first lol) d))
                (words-by* (rest lol) d)]
               [else (cons (words-by (first lol) d)
                           (words-by* (rest lol) d))])]))

; Letter Dictionary -> Dictionary
; produces a dictionary with all the words in the dictionary d with the letter l
(check-expect (words-by ""  DICTIONARY-EMPTY)   '())
(check-expect (words-by "e" DICTIONARY-EMPTY)   '())
(check-expect (words-by ""  DICTIONARY-AS-LIST) '())
(check-expect (words-by "d" DICTIONARY-AS-LIST) '())
(check-expect (words-by "e" DICTIONARY-AS-LIST)  (list "eco" "erlang"))

(define (words-by l d)
  (cond [(or (string=? l "")
             (empty? d))
         '()]
        [else
         (cond [(string=? (string-ith (first d) 0) l)
                (cons (first d)
                      (words-by l (rest d)))]
               [else (words-by l (rest d))])]))

; Dictionary -> Letter-Count
; produces the Letter-Count for the letter that is occurs most often
; as the first one in the given Dictionary
(check-expect (most-frequent.v2 DICTIONARY-EMPTY)  '())
(check-expect (most-frequent.v2 DICTIONARY-AS-LIST) (list "e" 2))
(check-expect (most-frequent    DICTIONARY-AS-LIST)
              (most-frequent.v2 DICTIONARY-AS-LIST))

(define (most-frequent.v2 d)
  (maximum-count.v3 (words-by-first-letter d) '()))

; LoD Letter-Count -> Letter-Count
; picks the pair with the maximum count
(check-expect (maximum-count.v3 (words-by-first-letter DICTIONARY-EMPTY)   '()) '())
(check-expect (maximum-count.v3 (words-by-first-letter DICTIONARY-AS-LIST) '())  (list "e" 2))

(define (maximum-count.v3 lolc max)
  (cond [(empty? lolc) max]
        [else
         (maximum-count.v3 (rest lolc)
                           (cond [(or (empty? max)
                                      (> (length (first lolc)) (second max)))
                                  (list (string-ith (first (first lolc)) 0)
                                        (length (first lolc)))]
                                 [else max]))]))



;; 12.2 - Real-World Data: iTunes

;; Exercise 199
;; Exercise 200
;; Exercise 201
;; Exercise 202
;; Exercise 203
;; Exercise 204
;; Exercise 205
;; Exercise 206
;; Exercise 207
;; Exercise 208


;; =================
;; Constants:

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")

; LTracks
; (define itunes-tracks (read-itunes-as-tracks ITUNES-LOCATION))

; LLists
; (define list-tracks (read-itunes-as-lists ITUNES-LOCATION))


;; =================
;; Data definitions:

; Date
(define DATE1 (create-date 1 2 3 4 5 6))
(define DATE2 (create-date 1 2 3 4 5 5))
(define DATE3 (create-date 6 5 4 3 2 1))
(define DATE4 (create-date 1 2 3 "four" 5 6))

; Track
(define TRACK1 (create-track "title A" "two" "three" 4 5 DATE1 7 DATE1))
(define TRACK2 (create-track "title B" "two" "three" 4 5 DATE3 7 DATE3))
(define TRACK3 (create-track "title C" "two" "new-three" 4 5 DATE3 7 DATE3))
(define TRACK4 (create-track "title D" "two" "new-three" 4 5 DATE3 7 DATE3))
(define TRACK5 (create-track "title A" "two" "three" 4 5 "a date" 7 "another date"))

; LTrack
(define LTRACK1 '())
(define LTRACK2 (list TRACK1))
(define LTRACK3 (list TRACK1 TRACK2))
(define LTRACK4 (list TRACK1 TRACK2 TRACK1))
(define LTRACK5 (list TRACK4 TRACK2 TRACK3 TRACK1))

; A List-of-strings is one of:
; - '()
; - (cons String List-of-strings)
; interpretation a list of String values

; A LoLT is one of:
; - '()
; - (cons LTracks LoLT)
; interpretation a collection of LTracks

; Association
(define A1 (list "Assoc A" "123ABC"))
(define A2 (list "Assoc B" 5))
(define A3 (list "Assoc C" 5.5))
(define A4 (list "Assoc D" (create-date 1 2 3 4 5 6)))
(define A5 (list "Assoc E" true))

; LAssoc
(define LASSOC1 '())
(define LASSOC2 (list A1 A2 A3 A4 A5))
(define LASSOC3 (list A5 A4 A3 A2 A1))

; LLists
(define LLIST1 '())
(define LLIST2 (list LASSOC2 LASSOC3))
(define LLIST3 (list LASSOC2 LASSOC3 LASSOC1))


;; =================
;; Functions:

; LTracks -> Number
; produces the total amount of play time
(check-expect (total-time LTRACK1) 0)
(check-expect (total-time LTRACK2) (+ 4 (/ 5 60) (/ 6 60 60)))
(check-expect (total-time LTRACK3) (+ 4 (/ 5 60) (/ 6 60 60)
                                      3 (/ 2 60) (/ 1 60 60)))

(define (total-time lt)
  (cond [(empty? lt) 0]
        [else
         (+ (date-hour (track-played (first lt)))
            (/ (date-minute (track-played (first lt))) 60)
            (/ (date-second (track-played (first lt))) 60 60)
            (total-time (rest lt)))]))

; LTracks -> List-of-strings
; produces the list of album titles
(check-expect (select-all-album-titles LTRACK1) '())
(check-expect (select-all-album-titles LTRACK2) (list "title A"))
(check-expect (select-all-album-titles LTRACK3) (list "title A" "title B"))
(check-expect (select-all-album-titles LTRACK4) (list "title A" "title B" "title A"))

(define (select-all-album-titles lt)
  (cond [(empty? lt) '()]
        [else (cons (track-name (first lt))
                    (select-all-album-titles (rest lt)))]))

; List-of-strings -> String
; constructs one that contains every String from the given list exactly once
(check-expect (create-set (select-all-album-titles LTRACK1)) "")
(check-expect (create-set (select-all-album-titles LTRACK2)) "title A")
(check-expect (create-set (select-all-album-titles LTRACK3)) "title A title B")
(check-expect (create-set (select-all-album-titles LTRACK4)) "title A title B")

(define (create-set los)
  (cond [(empty? los) ""]
        [(empty? (rest los)) (first los)]
        [else (string-append (first los) " "
                             (create-set (remove-all (first los) (rest los))))]))

; LTracks -> List-of-strings
; produces a list of unique album titles
(check-expect (select-album-titles/unique LTRACK1) '())
(check-expect (select-album-titles/unique LTRACK2) (list "title A"))
(check-expect (select-album-titles/unique LTRACK3) (list "title A" "title B"))
(check-expect (select-album-titles/unique LTRACK4) (list "title A" "title B"))

(define (select-album-titles/unique lt)
  (cond [(empty? lt) '()]
        [else (cons (track-name (first lt))
                    (select-album-titles/unique (remove-all (first lt) (rest lt))))]))

; String String LTracks -> LTracks
; extracts from the latter the list of tracks that belong to the given album
(check-expect (select-album "title A"  "three" LTRACK1) '())
(check-expect (select-album "title A"  "three" LTRACK3) (list TRACK1))
(check-expect (select-album "title B"  "three" LTRACK3) (list TRACK2))
(check-expect (select-album "title A"  "three" LTRACK4) (list TRACK1 TRACK1))
(check-expect (select-album "title B"  "three" LTRACK4) (list TRACK2))
(check-expect (select-album "title AB" "three" LTRACK2) '())

(define (select-album t a lt)
  (cond [(empty? lt) '()]
        [(and (string=? (track-name  (first lt)) t)
              (string=? (track-album (first lt)) a))
         (cons (first lt)
               (select-album t a (rest lt)))]
        [else (select-album t a (rest lt))]))

; String String Date LTracks -> LTracks
; extracts from the latter the list of tracks that belong
; to the given album and have been played after the given date
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK1) '())
(check-expect (select-album-date "title A"  "three" DATE3 LTRACK3) (list TRACK1))
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK3) '())
(check-expect (select-album-date "title A"  "three" DATE3 LTRACK4) (list TRACK1 TRACK1))
(check-expect (select-album-date "title A"  "three" DATE1 LTRACK4) '())
(check-expect (select-album-date "title AB" "three" DATE3 LTRACK3) '())

(define (select-album-date t a d lt)
  (cond [(empty? lt) '()]
        [(and (string=? (track-name  (first lt)) t)
              (string=? (track-album (first lt)) a)
              (date>? (track-played (first lt)) d))
         (cons (first lt)
               (select-album-date t a d (rest lt)))]
        [else (select-album-date t a d (rest lt))]))

; Date Date -> Boolean
; determines whether the first occurs before the second
(check-expect (date>? DATE1 DATE3) #true)
(check-expect (date>? DATE1 DATE1) #false)

(define (date>? d1 d2)
  (or
   (> (date-year   d1) (date-year   d2))
   (> (date-month  d1) (date-month  d2))
   (> (date-day    d1) (date-day    d2))
   (> (date-hour   d1) (date-hour   d2))
   (> (date-minute d1) (date-minute d2))
   (> (date-second d1) (date-second d2))))

; LTracks -> LoLT
; produce a list of LTracks, one per album
(check-expect (select-albums LTRACK1) '())
(check-expect (select-albums LTRACK2) (list (list TRACK1)))
(check-expect (select-albums LTRACK3) (list (list TRACK1 TRACK2)))
(check-expect (select-albums LTRACK4) (list (list TRACK1 TRACK2)))
(check-expect (select-albums LTRACK5) (list (list TRACK4 TRACK3)
                                            (list TRACK2 TRACK1)))

(define (select-albums lt)
  (cond [(empty? lt) '()]
        [else (cons (extract-album (track-album (first lt)) lt)
                    (select-albums (delete-album (track-album (first lt)) (rest lt))))]))

; String LTracks -> LTracks
; extracts the tracks belonging to the given album from the list of tracks
(check-expect (extract-album "three"     LTRACK1) '())
(check-expect (extract-album "three"     LTRACK3) (list TRACK1 TRACK2))
(check-expect (extract-album "new-three" LTRACK3) '())
(check-expect (extract-album "three"     LTRACK4) (list TRACK1 TRACK2))
(check-expect (extract-album "three"     LTRACK5) (list TRACK2 TRACK1))
(check-expect (extract-album "new-three" LTRACK5) (list TRACK4 TRACK3))

(define (extract-album a lt)
  (cond [(empty? lt) '()]
        [(string=? (track-album (first lt)) a)
         (cons (first lt)
               (extract-album a (remove (first lt) (rest lt))))]
        [else (extract-album a (rest lt))]))

; String LTracks -> LTracks
; removes the tracks belonging to the given album from the list of tracks
(check-expect (delete-album "three"     LTRACK1) '())
(check-expect (delete-album "three"     LTRACK3) '())
(check-expect (delete-album "new-three" LTRACK3) (list TRACK1 TRACK2))
(check-expect (delete-album "three"     LTRACK4) '())
(check-expect (delete-album "three"     LTRACK5) (list TRACK4 TRACK3))
(check-expect (delete-album "new-three" LTRACK5) (list TRACK2 TRACK1))

(define (delete-album a lt)
  (cond [(empty? lt) '()]
        [(string=? (track-album (first lt)) a)
         (delete-album a (remove (first lt) (rest lt)))]
        [else (cons (first lt)
                    (delete-album a (rest lt)))]))

; String LAssoc Any -> Association
; produces the first Association whose first item is
; equal to key or default if there is no such Association
(check-expect (find-association "Assoc A" LASSOC1 A5) A5)
(check-expect (find-association "Assoc A" LASSOC2 A1) A1)
(check-expect (find-association "Assoc A" LASSOC3 A1) A1)
(check-expect (find-association "Assoc F" LASSOC1 A1) A1)

(define (find-association key la default)
  (cond [(empty? la) default]
        [(string=? (first (first la)) key) (first la)]
        [else (find-association key (rest la) default)]))

; LLists -> Number
; produces the total amount of play time
(check-within (total-time/list LLIST1) 0 0.01)
(check-within (total-time/list LLIST2) 8.17 0.01)
(check-within (total-time/list LLIST3) 8.17 0.01)

(define (total-time/list ll)
  (cond [(empty? ll) 0]
        [else (+ (total-time/la   (first ll))
                 (total-time/list (rest  ll)))]))

; LAssoc -> Number
; produces the total amount of play time
(check-expect (total-time/la LASSOC1) 0)
(check-expect (total-time/la LASSOC2) (+ 4 (/ 5 60) (/ 6 60 60)))
(check-expect (total-time/la LASSOC3) (+ 4 (/ 5 60) (/ 6 60 60)))

(define (total-time/la la)
  (cond [(empty? la) 0]
        [(date? (second (first la)))
         (+ (date-hour (second (first la)))
            (/ (date-minute (second (first la))) 60)
            (/ (date-second (second (first la))) 60 60)
            (total-time/la (rest la)))]
        [else (total-time/la (rest la))]))

; LLists -> String
; produces the Strings that are associated with a Boolean attribute
(check-expect (boolean-attributes LLIST1) "")
(check-expect (boolean-attributes LLIST2) "Assoc E Assoc E")
(check-expect (boolean-attributes LLIST3) "Assoc E Assoc E")
(check-expect (boolean-attributes (list LASSOC2 (list A5 A2 A5))) "Assoc E Assoc E Assoc E")
(check-expect (boolean-attributes (list LASSOC2 (list A2 A3 A4))) "Assoc E")

(define (boolean-attributes ll)
  (cond [(empty? ll) ""]
        [(empty? (rest ll)) (create-set-la (delete-non-boolean (first ll)))]
        [else
         (string-append
          (create-set-la (delete-non-boolean (first ll)))
          (if (> (string-length (create-set-la (delete-non-boolean (first (rest ll))))) 0)
              " " "")
          (boolean-attributes (rest ll)))]))

; LAssoc -> LAssoc
; deletes the association with BSDN non-Boolean
(check-expect (delete-non-boolean LASSOC1) LASSOC1)
(check-expect (delete-non-boolean LASSOC2) (list A5))
(check-expect (delete-non-boolean LASSOC3) (list A5))
(check-expect (delete-non-boolean (list A5 A2 A5)) (list A5 A5))

(define (delete-non-boolean la)
  (cond [(empty? la) '()]
        [(boolean? (second (first la)))
         (append (list (first la))
                 (delete-non-boolean (rest la)))]
        [else (delete-non-boolean (rest la))]))

; LAssoc -> String
; constructs one that contains every String from the given list
(check-expect (create-set-la (delete-non-boolean LASSOC1)) "")
(check-expect (create-set-la (delete-non-boolean LASSOC2)) "Assoc E")
(check-expect (create-set-la (delete-non-boolean LASSOC3)) "Assoc E")
(check-expect (create-set-la (delete-non-boolean (list A5 A2 A5))) "Assoc E Assoc E")

(define (create-set-la la)
  (cond [(empty? la) ""]
        [(empty? (rest la)) (first (first la))]
        [else (string-append (first (first la)) " "
                             (create-set-la (rest la)))]))



;; 12.3 - Word Games, Composition Illustrated

;; Exercise 209
;; Exercise 210
;; Exercise 211
;; Exercise 212
;; ---
;; Exercise 214


;; =================
;; Data definitions:

; A 1String is a String of length 1,
; including
; - "\\" (the backslash),
; - " " (the space bar),
; - "\t" (tab),
; - "\r" (return), and
; - "\b" (backspace).
; interpretation represents keys on the keyboard

; A Word is one of:
; - '() or
; - (cons 1String Word)
; interpretation a String as a list of 1Strings (letters)

; A List-of-words is one of:
; - '() or
; - (cons Word List-of-words)
; interpretation a collection of Word values


;; =================
;; Functions:

; List-of-strings -> Boolean
(define (all-words-from-rat? w)
  (and (member? "rat" w)
       (member? "art" w)
       (member? "tar" w)))

; String -> List-of-strings
; finds all words that the letters of some given word spell
(check-member-of (alternative-words "cat")
                 (list "act" "cat")
                 (list "cat" "act"))
(check-satisfied (alternative-words "rat")
                 all-words-from-rat?)

(define (alternative-words s)
  (in-dictionary
   (words->strings (arrangements (string->word s)))))

; Word -> List-of-words
; creates all rearrangements of the letters in w
(check-expect (arrangements '()) (list '()))
(check-expect (arrangements (list "d" "e"))
              (list (list "d" "e")
                    (list "e" "d")))

(define (arrangements w)
  (cond [(empty? w) (list '())]
        [else
         (insert-everywhere/in-all-words (first w)
                                         (arrangements (rest w)))]))

; String -> Word
; convert s to the chosen word representation
(check-expect (string->word "") '())
(check-expect (string->word "hello") (list "h" "e" "l" "l" "o"))

(define (string->word s)
  (explode s))

; Word -> String
; convert w to a string
(check-expect (word->string '()) "")
(check-expect (word->string (list "h" "e" "l" "l" "o")) "hello")

(define (word->string w)
  (implode w))

; List-of-words -> List-of-strings
; turn all Words in low into Strings
(check-expect (words->strings '()) '())
(check-expect (words->strings (list (explode "hello")
                                    (explode "world")))
              (list "hello" "world"))

(define (words->strings low)
  (cond [(empty? low) '()]
        [else (cons (word->string   (first low))
                    (words->strings (rest low)))]))

; List-of-strings -> List-of-strings
; pick out all those Strings that occur in the dictionary
(check-expect (in-dictionary '()) '())
(check-expect (in-dictionary (list "hello" "world")) (list "hello" "world"))
(check-expect (in-dictionary (list "a" "car")) '())

(define (in-dictionary los)
  (cond [(empty? los) '()]
        [(member? (first los) (list "rat" "art" "tar" "act" "cat" "hello" "world"))
         (cons (first los)
               (in-dictionary (rest los)))]
        [else  (in-dictionary (rest los))]))



;; 12.4 - Word Games, the Heart of the Problem

;; Exercise 213


;; =================
;; Functions:

; 1String List-of-words -> List-of-words
; result is a list of words like its second argument,
; but with the first argument inserted at the beginning,
; between all letters, and at the end of all words of the given list
(check-expect (insert-everywhere/in-all-words "d" '()) '())
(check-expect (insert-everywhere/in-all-words "d" (list (list "e")))
              (list (list "d" "e")
                    (list "e" "d")))
(check-expect (insert-everywhere/in-all-words "d" (list (list "e" "r")
                                                        (list "r" "e")))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")
                    (list "d" "r" "e")
                    (list "r" "d" "e")
                    (list "r" "e" "d")))

(define (insert-everywhere/in-all-words 1s low)
  (cond [(empty? low) '()]
        [else
         (append (insert-everywhere/in-word 1s '()  (first low))
                 (insert-everywhere/in-all-words 1s (rest  low)))]))

; 1String Word Word -> List-of-words
; arrangements the words (prefix sp and suffix ss) with 1String 1s
(check-expect (insert-everywhere/in-word "d" '() '())
              (list (list "d")))
(check-expect (insert-everywhere/in-word "d" '() (list "e" "r"))
              (list (list "d" "e" "r")
                    (list "e" "d" "r")
                    (list "e" "r" "d")))

(define (insert-everywhere/in-word 1s sp ss)
  (cond [(empty? ss) (list (append sp (list 1s) ss))]
        [else
         (cons (append sp (list 1s) ss)
               (insert-everywhere/in-word 1s
                                          (append sp (list (first ss)))
                                          (rest ss)))]))



;; 12.5 - Feeding Worms

;; Exercise 215
;; Exercise 216
;; Exercise 217
;; Exercise 218
;; Exercise 219


;; =================
;; Constants:

(define SIZE 10)
(define SEGMENT (circle (/ SIZE 2) "solid" "red"))
(define FOOD    (circle (/ SIZE 2) "solid" "green"))
(define WIDTH  600)
(define HEIGHT 400)
(define BACKGROUND (empty-scene WIDTH HEIGHT))


;; =================
;; Data definitions:

; A Direction is one of:
; - "left"
; - "up"
; - "right"
; - "down"
; interpretation these strings represent the directions on the screen

(define-struct tail (left top))
; A Tail is a structure:
;   (make-tail Number Number)
; interpretation (make-tail l t) represents a position left l and top t

; A Tails is one of:
; '()
; '(cons Tail Tails)
; interpretation represents the tail of the snake

(define-struct snake (left top direction tails food))
; A Snake is a structure:
;   (make-snake Number Number Direction Tails Posn)
; interpretation (make-snake l t d t f) represents a position left l and top t;
; the direction of movement d, the tails t and the food f position
(define S0 (make-snake (/ WIDTH 2) (/ HEIGHT 2) "down" '() (make-posn 100 10)))
(define S1 (make-snake 10 10 "left"  '() null))
(define S2 (make-snake 10 10 "up"    '() null))
(define S3 (make-snake 10 10 "right" '() null))
(define S4 (make-snake 10 10 "down"  '() null))
(define S5 (make-snake 10 10 "down"   (list (make-tail 0  0)
                                            (make-tail 10 0))
                       (make-posn 10 50)))
(define S6 (make-snake 10 10 "up" (list (make-tail 10 10)
                                        (make-tail 20 10)
                                        (make-tail 20 20)
                                        (make-tail 10 20))
                       null))
(define S7 (make-snake 10 10 "down" (list (make-tail 0  0)
                                          (make-tail 10 0))
                       (make-posn 10 20)))
(define S8 (make-snake 10 10 "down" '() (make-posn 10 20)))
(define SM S0)


;; =================
;; Functions:

; Snake -> Snake
; starts a world with (worm-main SM)
(define (worm-main s)
  (big-bang s
            (on-tick tock 1)
            (to-draw render)
            (on-key  event)
            (stop-when game-over? game-over)))

; Snake -> Snake
; updates the position of the snake with the current direction
(check-expect (tock S5)
              (make-snake (future-left S5)
                          (future-top S5)
                          (snake-direction S5)
                          (list (make-tail 10 0)
                                (make-tail 10 10))
                          (snake-food S5)))
(check-random (tock S7)
              (make-snake (future-left S7)
                          (future-top S7)
                          (snake-direction S7)
                          (list (make-tail 0  0)
                                (make-tail 10 0)
                                (make-tail 10 10))
                          (food-create (make-posn (future-left S7) (future-top S7)))))
(check-random (tock S8)
              (make-snake (future-left S8)
                          (future-top S8)
                          (snake-direction S8)
                          (list (make-tail 10 10))
                          (food-create (make-posn (future-left S8) (future-top S8)))))
(check-expect (tock S0)
              (make-snake (future-left S0)
                          (future-top S0)
                          (snake-direction S0)
                          '()
                          (snake-food S0)))

(define (tock s)
  (make-snake
   (future-left s)
   (future-top  s)
   (snake-direction s)
   (cond [(eat? (future-left s) (future-top s) (snake-food s))
          (append (snake-tails s)
                  (list (make-tail (snake-left s)
                                   (snake-top  s))))]
         [(empty? (snake-tails s)) '()]
         [else (append (rest (snake-tails s))
                       (list (make-tail (snake-left s)
                                        (snake-top  s))))])
   (cond [(eat? (future-left s) (future-top s) (snake-food s))
          (food-create (make-posn (future-left s)
                                  (future-top s)))]
         [else (snake-food s)])))

; Snake -> Number
; calculates the future value for the left position
(check-expect (future-left S1) (- (snake-left S1) SIZE))
(check-expect (future-left S2) (snake-left S2))
(check-expect (future-left S3) (+ (snake-left S3) SIZE))
(check-expect (future-left S4) (snake-left S4))

(define (future-left s)
  (cond [(string=? (snake-direction s) "left")  (- (snake-left s) SIZE)]
        [(string=? (snake-direction s) "right") (+ (snake-left s) SIZE)]
        [else (snake-left s)]))

; Snake -> Number
; calculates the future value for the top position
(check-expect (future-top S1) (snake-top S1))
(check-expect (future-top S2) (- (snake-top S2) SIZE))
(check-expect (future-top S3) (snake-top S3))
(check-expect (future-top S4) (+ (snake-top S4) SIZE))

(define (future-top s)
  (cond [(string=? (snake-direction s) "up")   (- (snake-top s) SIZE)]
        [(string=? (snake-direction s) "down") (+ (snake-top s) SIZE)]
        [else (snake-top s)]))

; Number Number Posn -> Boolean
; checks if the distance from the left and top position is near the food position in SIZE
(check-expect (eat? 0 0 (make-posn 0   0))   #true)
(check-expect (eat? 0 0 (make-posn 0   10))  #true)
(check-expect (eat? 0 0 (make-posn 0   -10)) #true)
(check-expect (eat? 0 0 (make-posn 10  0))   #true)
(check-expect (eat? 0 0 (make-posn -10 0))   #true)
(check-expect (eat? 0 0 (make-posn 10  10))  #false)

(define (eat? l t p)
  (<= (sqrt (+ (sqr (- (posn-x p) l))
               (sqr (- (posn-y p) t))))
      SIZE))

; Snake -> Image
; renders the snake on the BACKGROUND
(define (render s)
  (place-image FOOD
               (+ (posn-x (snake-food s)) (/ SIZE 2))
               (+ (posn-y (snake-food s)) (/ SIZE 2))
               (place-image SEGMENT
                            (+ (snake-left s) (/ SIZE 2))
                            (+ (snake-top s)  (/ SIZE 2))
                            (render-tails (snake-tails s)))))

; Tails -> Image
; renders the tails on the BACKGROUND
(define (render-tails t)
  (cond [(empty? t) BACKGROUND]
        [else
         (place-image SEGMENT
                      (+ (tail-left (first t)) (/ SIZE 2))
                      (+ (tail-top  (first t)) (/ SIZE 2))
                      (render-tails (rest t)))]))

; Snake KeyEvent -> Snake
; updates the direction of snake with Direction
(check-expect (event S1 " ")     S1)
(check-expect (event S1 "left")  S1)
(check-expect (event S1 "up")    S2)
(check-expect (event S1 "right") S3)
(check-expect (event S1 "down")  S4)
(check-expect (event S7 "up")    S7)

(define (event s ke)
  (make-snake (snake-left s)
              (snake-top s)
              (cond [(and (not (empty? (snake-tails s)))
                          (or (and (key=? ke "left")
                                   (string=? (snake-direction s) "right"))
                              (and (key=? ke "right")
                                   (string=? (snake-direction s) "left"))
                              (and (key=? ke "up")
                                   (string=? (snake-direction s) "down"))
                              (and (key=? ke "down")
                                   (string=? (snake-direction s) "up"))))
                     (snake-direction s)]
                    [(or (key=? ke "left")
                         (key=? ke "up")
                         (key=? ke "right")
                         (key=? ke "down")) ke]
                    [else (snake-direction s)])
              (snake-tails s)
              (snake-food  s)))

; Snake -> Boolean
; stops if the snake has run into the walls of the world
(check-expect (hit-border? (make-snake 0  10 "left"  '() null)) #true)
(check-expect (hit-border? (make-snake 0  10 "up"    '() null)) #false)
(check-expect (hit-border? (make-snake 0  10 "right" '() null)) #false)
(check-expect (hit-border? (make-snake 0  10 "down"  '() null)) #false)
(check-expect (hit-border? (make-snake 10 0 "up"     '() null)) #true)
(check-expect (hit-border? (make-snake 10 0 "right"  '() null)) #false)
(check-expect (hit-border? (make-snake 10 0 "down"   '() null)) #false)
(check-expect (hit-border? (make-snake 10 0 "left"   '() null)) #false)
(check-expect (hit-border? (make-snake (- WIDTH SIZE) 10 "right"  '() null)) #true)
(check-expect (hit-border? (make-snake (- WIDTH SIZE) 10 "down"   '() null)) #false)
(check-expect (hit-border? (make-snake (- WIDTH SIZE) 10 "left"   '() null)) #false)
(check-expect (hit-border? (make-snake (- WIDTH SIZE) 10 "up"     '() null)) #false)
(check-expect (hit-border? (make-snake 10 (- HEIGHT SIZE) "down"  '() null)) #true)
(check-expect (hit-border? (make-snake 10 (- HEIGHT SIZE) "left"  '() null)) #false)
(check-expect (hit-border? (make-snake 10 (- HEIGHT SIZE) "up"    '() null)) #false)
(check-expect (hit-border? (make-snake 10 (- HEIGHT SIZE) "right" '() null)) #false)
(check-expect (hit-border? S1) #false)
(check-expect (hit-border? S6) #true)

(define (hit-border? s)
  (or (and (= (snake-left s) 0)
           (string=? (snake-direction s) "left"))
      (and (= (snake-top s)  0)
           (string=? (snake-direction s) "up"))
      (and (= (snake-left s) (- WIDTH SIZE))
           (string=? (snake-direction s) "right"))
      (and (= (snake-top s)  (- HEIGHT SIZE))
           (string=? (snake-direction s) "down"))
      (member? (make-tail (snake-left s) (snake-top s)) (snake-tails s))))

; Snake -> Boolean
; stops if the snake has run into itself
(check-expect (hit-itself? S1) #false)
(check-expect (hit-itself? S5) #false)
(check-expect (hit-itself? S6) #false)
(check-expect (hit-itself? S6) #false)
(check-expect (hit-itself? (make-snake 0 0 "down" (list (make-tail 0  0)
                                                        (make-tail 10 0)
                                                        (make-tail 10 10)
                                                        (make-tail 0  10)
                                                        (make-tail 0  0))
                                       null))
              #true)

(define (hit-itself? s)
  (cond [(empty? (snake-tails s)) #false]
        [else
         (member? (first (snake-tails s))
                  (rest  (snake-tails s)))]))

; Snake -> Boolean
; stops if the snake has run into the walls of the world or itself
(define (game-over? s)
  (or (hit-border? s)
      (hit-itself? s)))

; Snake -> Image
; renders the game over message on the last render
(define (game-over s)
  (place-image
   (text (if (hit-itself? s)
             "worm hit itself"
             "worm hit the wall")
         12 "black")
   50 (- HEIGHT 10)
   (render s)))

; Posn -> Posn
; creates a food randomly between WIDTH and HEIGHT
(check-satisfied (food-create (make-posn 1 1)) not-equal-1-1?)

(define (food-create p)
  (food-check-create p (make-posn (random WIDTH) (random HEIGHT))))

; Posn Posn -> Posn
; generative recursion
; checks if the food exists, otherwise it creates a new food
(define (food-check-create p candidate)
  (if (equal? p candidate) (food-create p) candidate))

; Posn -> Boolean
; use for testing only
(define (not-equal-1-1? p)
  (not (and (= (posn-x p) 1) (= (posn-y p) 1))))



;; 12.6 - Simple Tetris

;; Exercise 220
;; Exercise 221
;; Exercise 222
;; Exercise 223


;; =================
;; Constants:

(define WIDTH.V2 10) ; # of blocks, horizontally
(define SIZE.V2  10) ; blocks are squares
(define SCENE-SIZE (* WIDTH.V2 SIZE.V2))
(define BACKGROUND.V2 (empty-scene SCENE-SIZE SCENE-SIZE))

; red squares with black rims
(define BLOCK
  (overlay (square (- SIZE.V2 1) "solid" "red")
           (square SIZE.V2 "outline" "black")))


;; =================
;; Data definitions:

(define-struct tetris [block landscape])
(define-struct block  [x y])

; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of:
; - '()
; - (cons Block Landscape)
; A Block is a structure:
;   (make-block N N)

; interpretations
; (make-block x y) depicts a block whose left
; corner is (* x SIZE.V2) pixels from the left and
; (* y SIZE.V2) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and ... are resting
(define landscape0 (list (make-block 0 0)
                         (make-block 0 (* 9 SIZE.V2))))
(define block-dropping (list (make-block 0 1)
                             (make-block 0 (* 9 SIZE.V2))))
(define tetris0 (make-tetris (make-block 0 0)
                             (list (make-block 10 (* 9 SIZE.V2))
                                   (make-block 0  (* 9 SIZE.V2)))))
(define tetris0-blocked (make-tetris (make-block 0 0)
                                     (list (make-block (* 1 SIZE.V2) 0))))
(define tetris0-drop (make-tetris (make-block 0 (* 8 SIZE.V2))
                                  (list (make-block 10 (* 9 SIZE.V2))
                                        (make-block 0  (* 9 SIZE.V2)))))
(define tetris0-drop-floor (make-tetris (make-block 0 (* 9 SIZE.V2)) '()))
(define tetris9 (make-tetris (make-block (* 9 SIZE.V2) 0) '()))
(define tetris9-blocked (make-tetris (make-block (* 9 SIZE.V2) 0)
                                     (list (make-block (* 8 SIZE.V2) 0))))
(define tetris0-game-over (make-tetris (make-block 0 0)
                                       (list (make-block 0 0))))
(define block-landed   (make-block 0 (- SCENE-SIZE 1)))
(define block-on-block (make-block 0 (- SCENE-SIZE 2)))


;; =================
;; Functions:

; Tetris -> Tetris
; starts a world with (tetris-main (make-tetris (make-block (* 5 SIZE.V2) 0) '()))
(define (tetris-main t)
  (big-bang t
            (on-tick   tetris-tock 0.1)
            (to-draw   tetris-render)
            (on-key    tetris-move)
            (stop-when tetris-over)))

; Tetris -> Tetris
; updates the Tetris states
(check-expect (tetris-tock tetris0)
              (make-tetris (make-block 0 (* 1 SIZE.V2))
                           (list (make-block 10 (* 9 SIZE.V2))
                                 (make-block 0  (* 9 SIZE.V2)))))
(check-expect (tetris-tock tetris0-drop)
              (make-tetris (make-block (* 1 SIZE.V2) 0)
                           (list (make-block 0  (* 8 SIZE.V2))
                                 (make-block 10 (* 9 SIZE.V2))
                                 (make-block 0  (* 9 SIZE.V2)))))
(check-expect (tetris-tock tetris0-drop-floor)
              (make-tetris (make-block (* 1 SIZE.V2) 0)
                           (list (make-block 0 (* 9 SIZE.V2)))))

(define (tetris-tock t)
  (make-tetris
   (cond [(drop? t) (make-block (block-new (block-x (tetris-block t))) 0)]
         [else (next-block (tetris-block t))])
   (cond [(drop? t) (cons (tetris-block t) (tetris-landscape t))]
         [else (tetris-landscape t)])))

; Tetris -> Boolean
; checks if the next block is dropped on the landscape or floor
(define (drop? t)
  (or (member? (next-block (tetris-block t)) (tetris-landscape t))
      (= (block-y (tetris-block t)) (* 9 SIZE.V2))))

; Block -> Block
; suggests the next block
(define (next-block b)
  (make-block (block-x b) (+ (block-y b) SIZE.V2)))

; Number -> Number
; generates the column to the right of the current one;
; else the left-most when in the limit (* 9 SIZE.V2)
(define (block-new c)
  (if (= c (* 9 SIZE.V2)) 0 (+ c SIZE.V2)))

; Number -> Number
; randomly selects a column different from the current one
(check-satisfied (block-generate 5) not-equal-5?)

(define (block-generate c)
  (block-check-generate c (random SIZE.V2)))

; Number Number -> Number
; generative recursion
; checks if the column exists, otherwise it creates a new column
(define (block-check-generate c candidate)
  (if (= c candidate) (block-generate c) candidate))

; Number -> Boolean
; use for testing only
(define (not-equal-5? p)
  (not (= p 5)))

; Tetris -> Image
; renders the tetris on the BACKGROUND.V2
(define (tetris-render t)
  (place-image/align BLOCK
                     (block-x (tetris-block t)) (block-y (tetris-block t))
                     "left" "top"
                     (landscape-render (tetris-landscape t))))

; Landscape -> Image
; renders the landscape on the BACKGROUND.V2
(define (landscape-render l)
  (cond [(empty? l) BACKGROUND.V2]
        [else
         (place-image/align BLOCK
                            (block-x (first l)) (block-y (first l))
                            "left" "top"
                            (landscape-render (rest l)))]))

; Tetris KeyEvent -> Tetris
; controls the horizontal movement of the dropping block
(check-expect (tetris-move tetris0 " ") tetris0)
(check-expect (tetris-move tetris0-blocked "left")  tetris0-blocked)
(check-expect (tetris-move tetris0-blocked "right") tetris0-blocked)
(check-expect (tetris-move tetris9-blocked "left")  tetris9-blocked)
(check-expect (tetris-move tetris9-blocked "right") tetris9-blocked)
(check-expect (tetris-move tetris0 "right")
              (make-tetris (make-block (* 1 SIZE.V2) 0)
                           (list (make-block 10 (* 9 SIZE.V2))
                                 (make-block 0  (* 9 SIZE.V2)))))
(check-expect (tetris-move tetris9 "left")
              (make-tetris (make-block (* 8 SIZE.V2) 0) '()))

(define (tetris-move t ke)
  (make-tetris
   (cond [(key=? ke "left")
          (cond [(= (block-x (tetris-block t)) 0) (tetris-block t)]
                [(member? (block-other t -1) (tetris-landscape t)) (tetris-block t)]
                [else (block-other t -1)])]
         [(key=? ke "right")
          (cond [(= (block-x (tetris-block t)) (* 9 SIZE.V2)) (tetris-block t)]
                [(member? (block-other t 1) (tetris-landscape t)) (tetris-block t)]
                [else (block-other t 1)])]
         [else (tetris-block t)])
   (tetris-landscape t)))

; Tetris -> Block
; creates a block in the left (-1) or right (1) column
(define (block-other t p)
  (make-block (+ (block-x (tetris-block t)) (* p SIZE.V2))
              (block-y (tetris-block t))))

; Tetris -> Boolean
; checks if the current block is unable to move
(check-expect (tetris-over tetris0) #false)
(check-expect (tetris-over tetris0-game-over) #true)

(define (tetris-over t)
  (member? (tetris-block t) (tetris-landscape t)))



;; 12.7 - Full Space War

;; Exercise 224


;; =================
;; Constants:

(define UFO (overlay (rectangle 70 10 "solid" "green")
                     (circle 20 "solid" "green")))
(define UFO-X (/ (image-width  UFO) 2))
(define UFO-Y (/ (image-height UFO) 2))
(define TANK-HEIGHT 20)
(define TANK    (rectangle 50 TANK-HEIGHT "solid" "blue"))
(define MISSILE (triangle 10 "solid" "red"))
(define WIDTH.V3  400)
(define HEIGHT.V3 300)
(define CLOSE (/ HEIGHT.V3 3))
(define BACKGROUND.V3 (empty-scene WIDTH.V3 HEIGHT.V3))


;; =================
;; Data definitions:

; An UFO is a Posn:
;   (make-posn Natural Natural)
; interpretation (make-posn x y) is the UFO's location
; (using the top-down, left-to-right convention)
(define U1 (make-posn 10 20))

(define-struct tank [loc vel])
; A Tank is a structure:
;   (make-tank Number Number)
; interpretation (make-tank x dx) specifies the position:
; (x, HEIGHT.V3) and the tank's speed: dx pixels/tick
(define T-0L (make-tank 0 -1))           ; Tank 0 Left
(define T-0R (make-tank 0 1))            ; Tank 0 Right
(define T-ML (make-tank (/ WIDTH.V3 2) -1)) ; Tank Middle Left
(define T-MR (make-tank (/ WIDTH.V3 2)  1)) ; Tank Middle Right
(define T-WL (make-tank WIDTH.V3 -1))       ; Tank Width Left
(define T-WR (make-tank WIDTH.V3  1))       ; Tank Width Right

; A Missile is a Posn:
;   (make-posn Natural Natural)
; interpretation (make-posn x y) is the Missile's location
; (using the top-down, left-to-right convention)
(define M1 (make-posn 10 50))
(define M2 (make-posn 50 20))

; A List-of-Missile is one of:
; '()
; (cons Missile List-of-Missile)
; interpretation represents the missiles launched
(define LOM1 '())
(define LOM2 (list M1 M2))

(define-struct sigs [ufo tank missiles])
; A SIGS is a structure:
;   (make-sigs UFO Tank List-of-Missile)
; interpretation represents the complete state of a space invader game
(define SIGS1 (make-sigs U1 T-MR LOM1))
(define SIGS2 (make-sigs U1 T-MR LOM2))
(define SIGS3 (make-sigs (make-posn 10 20)
                         (make-tank 28 -3)
                         (list (make-posn 32 (- HEIGHT.V3 TANK-HEIGHT 10)))))
(define SIGS4 (make-sigs (make-posn 20 100)
                         (make-tank 100 3)
                         (list (make-posn 22 120))))
(define SIGS5 (make-sigs (make-posn 10 (- HEIGHT.V3 CLOSE))
                         (make-tank 28 -3)
                         '()))


;; =================
;; Functions:

; SIGS -> World
; starts a world with (sigs-main SIGS1)
(define (sigs-main s)
  (big-bang s
            (on-tick   si-move)
            (on-draw   si-render)
            (on-key    si-control)
            (stop-when si-game-over?)))

; SIGS -> SIGS
; updates the position of objects
(define (si-move s)
  (make-sigs (update-ufo      (sigs-ufo s))
             (update-tank     (sigs-tank s))
             (update-missiles (sigs-missiles s))))

; UFO Number -> UFO
; updates the position of UFO
(check-random (update-ufo U1)
              (make-posn (random (+ (posn-x U1) (image-width UFO)))
                         (add1 (posn-y U1))))

(define (update-ufo u)
  (make-posn (random (+ (posn-x u) (image-width UFO)))
             (add1 (posn-y u))))

; Tank -> Tank
; updates the position of Tank
(check-expect (update-tank T-0L) (make-tank 0 -1))
(check-expect (update-tank T-0R) (make-tank 1 1))
(check-expect (update-tank T-ML) (make-tank (sub1 (/ WIDTH.V3 2)) -1))
(check-expect (update-tank T-MR) (make-tank (add1 (/ WIDTH.V3 2))  1))
(check-expect (update-tank T-WL) (make-tank (sub1 WIDTH.V3) -1))
(check-expect (update-tank T-WR) (make-tank WIDTH.V3  1))

(define (update-tank t)
  (make-tank (cond [(<   (+ (tank-loc t) (tank-vel t)) 0) 0]
                   [(>   (+ (tank-loc t) (tank-vel t)) WIDTH.V3) WIDTH.V3]
                   [else (+ (tank-loc t) (tank-vel t))])
             (tank-vel t)))

; List-of-Missile -> List-of-Missile
; updates the position of missiles
(check-expect (update-missiles LOM1) '())
(check-expect (update-missiles LOM2) (list (make-posn 10 49)
                                           (make-posn 50 19)))

(define (update-missiles lom)
  (cond [(empty? lom) '()]
        [else (cons (update-missile (first lom))
                    (update-missiles (rest lom)))]))

; Missile -> Missile
; updates the position of missile
(check-expect (update-missile M1) (make-posn 10 49))
(check-expect (update-missile M2) (make-posn 50 19))

(define (update-missile m)
  (make-posn (posn-x m) (sub1 (posn-y m))))

; SIGS -> Image
; renders the given game state on top of BACKGROUND.V3
(check-expect (si-render SIGS1)
              (place-image UFO
                           (posn-x (sigs-ufo SIGS1)) (posn-y (sigs-ufo SIGS1))
                           (place-image TANK (tank-loc (sigs-tank SIGS1)) HEIGHT.V3 BACKGROUND.V3)))
(check-expect (si-render SIGS2)
              (place-image UFO
                           (posn-x (sigs-ufo SIGS2)) (posn-y (sigs-ufo SIGS2))
                           (place-image TANK (tank-loc (sigs-tank SIGS2)) HEIGHT.V3
                                        (place-image MISSILE (posn-x (first (sigs-missiles SIGS2))) (posn-y (first (sigs-missiles SIGS2)))
                                                     (place-image MISSILE (posn-x (second (sigs-missiles SIGS2))) (posn-y (second (sigs-missiles SIGS2))) BACKGROUND.V3)))))

(define (si-render s)
  (ufo-render (sigs-ufo s)
              (tank-render (sigs-tank s)
                           (missiles-render (sigs-missiles s) BACKGROUND.V3))))

; UFO Image -> Image
; adds u to the given image im
(define (ufo-render u im)
  (place-image UFO (posn-x u) (posn-y u) im))

; Tank Image -> Image
; adds t to the given image im
(define (tank-render t im)
  (place-image TANK (tank-loc t) HEIGHT.V3 im))

; List-of-Missile Image -> Image
; adds the missiles lom to the given image im
(define (missiles-render lom im)
  (cond [(empty? lom) im]
        [else (place-image MISSILE (posn-x (first lom)) (posn-y (first lom))
                           (missiles-render (rest lom) im))]))

; SIGS KeyEvent -> SIGS
; handles the main events:
; - pressing the left arrow ensures that the tank moves left;
; - pressing the right arrow ensures that the tank moves right; and
; - pressing the space bar fires a new missile.
(check-expect (si-control SIGS1 "up") SIGS1)
(check-expect (si-control SIGS1 "left")
              (make-sigs (sigs-ufo SIGS1)
                         (make-tank (tank-loc (sigs-tank SIGS1))
                                    (* -1 (tank-vel (sigs-tank SIGS1))))
                         (sigs-missiles SIGS1)))
(check-expect (si-control SIGS1 "right")
              (make-sigs (sigs-ufo SIGS1)
                         (make-tank (tank-loc (sigs-tank SIGS1))
                                    (tank-vel (sigs-tank SIGS1)))
                         (sigs-missiles SIGS1)))
(check-expect (si-control SIGS1 " ")
              (make-sigs (sigs-ufo SIGS1)
                         (sigs-tank SIGS1)
                         (list (make-posn (tank-loc (sigs-tank SIGS1))
                                          (- HEIGHT.V3 TANK-HEIGHT)))))

(define (si-control s ke)
  (make-sigs (sigs-ufo s)
             (cond [(or (key=? ke "left")
                        (key=? ke "right"))
                    (tank-direction (sigs-tank s) ke)]
                   [else (sigs-tank s)])
             (cond [(key=? ke " ")
                    (missiles-fire (tank-loc (sigs-tank s))
                                   (sigs-missiles s))]
                   [else (sigs-missiles s)])))

; Tank String -> Tank
; changes the speed direction of tank
(check-expect (tank-direction T-ML "left")  T-ML)
(check-expect (tank-direction T-ML "right") T-MR)
(check-expect (tank-direction T-MR "left")  T-ML)
(check-expect (tank-direction T-MR "right") T-MR)

(define (tank-direction t s)
  (make-tank (tank-loc t)
             (cond [(string=? s "left")  (* -1 (abs (tank-vel t)))]
                   [(string=? s "right") (abs (tank-vel t))]
                   [else t])))

; List-of-Missile -> List-of-Missile
; fires a missile at the coordinate: x and (- HEIGHT.V3 TANK-HEIGHT)
(check-expect (missiles-fire 10 LOM1)
              (list (make-posn 10 (- HEIGHT.V3 TANK-HEIGHT))))
(check-expect (missiles-fire 30 LOM2)
              (list M1 M2 (make-posn 30 (- HEIGHT.V3 TANK-HEIGHT))))

(define (missiles-fire x lom)
  (append lom (cons (make-posn x (- HEIGHT.V3 TANK-HEIGHT)) '())))

; SIGS -> Boolean
; returns true when the game stop;
; the game stops if the UFO lands or if the missile hits the UFO
(check-expect (si-game-over? SIGS1) #false)
(check-expect (si-game-over? SIGS2) #false)
(check-expect (si-game-over? SIGS3) #false)
(check-expect (si-game-over? SIGS4) #true)
(check-expect (si-game-over? SIGS5) #true)

(define (si-game-over? s)
  (cond [(>= (posn-y (sigs-ufo s))
             (- HEIGHT.V3 CLOSE)) #true]
        [else (hit-missile? (sigs-missiles s) (sigs-ufo s))]))

; List-of-Missile UFO -> Boolean
; checks if any missiles hit the UFO
(define (hit-missile? lom u)
  (cond [(empty? lom) #false]
        [(and (<= (- (posn-x u) UFO-X)
                  (posn-x (first lom))
                  (+ (posn-x u) UFO-X))
              (<= (- (posn-y u) UFO-Y)
                  (posn-y (first lom))
                  (+ (posn-y u) UFO-Y))) #true]
        [else (hit-missile? (rest lom) u)]))

;; Exercise 225


;; =================
;; Constants:

(define AIRPLANE (overlay/xy (circle 10 "solid" "blue")
                             -15 -20
                             (overlay (ellipse 10 80 "solid" "blue")
                                      (rectangle 50 10 "solid" "blue"))))
(define TREE (overlay (above (circle 5 "solid" "forestgreen")
                             (beside (circle 5 "solid" "forestgreen")
                                     (circle 5 "solid" "forestgreen")))
                      (circle 14 "outline" "black")))
(define FOREST (beside TREE (above TREE TREE)))
(define FIRE (overlay (circle 4 75 "red")
                      (overlay (circle 8 75 "orange")
                               (circle 12 75 "yellow"))))
(define WATER (above (ellipse 8 10 "solid" "blue")
                     (ellipse 8 10 "solid" "blue")
                     (ellipse 8 10 "solid" "blue")
                     (ellipse 8 10 "solid" "blue")
                     (ellipse 8 10 "solid" "blue")))
(define WIDTH.V4  640)
(define HEIGHT.V4 WIDTH.V4)
(define CENTER-X (/ WIDTH.V4 2))
(define CENTER-Y (/ HEIGHT.V4 2))
(define FIRES 8)
(define WATER-LOAD 5)
(define TOCK-STEP 28) ; tock: 28 times by second
(define TIMEOUT (* 120 TOCK-STEP))
(define CTR-Y (- HEIGHT.V4 (/ (image-height AIRPLANE) 2)))
(define STEP-X 5)
(define MTS
  (place-image FOREST (* WIDTH.V4 0.2) (* HEIGHT.V4 0.2)
               (place-image FOREST (* WIDTH.V4 0.2) (* HEIGHT.V4 0.5)
                            (place-image FOREST (* WIDTH.V4 0.2) (* HEIGHT.V4 0.8)
                                         (place-image FOREST (* WIDTH.V4 0.5) (* HEIGHT.V4 0.2)
                                                      (place-image FOREST (* WIDTH.V4 0.5) (* HEIGHT.V4 0.5)
                                                                   (place-image FOREST (* WIDTH.V4 0.5) (* HEIGHT.V4 0.8)
                                                                                (place-image FOREST (* WIDTH.V4 0.8) (* HEIGHT.V4 0.2)
                                                                                             (place-image FOREST (* WIDTH.V4 0.8) (* HEIGHT.V4 0.5)
                                                                                                          (place-image FOREST (* WIDTH.V4 0.8) (* HEIGHT.V4 0.8)
                                                                                                                       (empty-scene WIDTH.V4 HEIGHT.V4 "mediumgoldenrod")))))))))))


;; =================
;; Data definitions:

; A Fire is a Posn:
;   (make-posn Natural Natural)
; interpretation (make-posn x y) is the Fire location
; (using the top-down, left-to-right convention)
(define F1 (make-posn 10 20))
(define F2 (make-posn 50 50))

; A ListOfFire is one of:
;   '()
;   (cons Fire ListOfFire)
; interpretation represents all fires in the game
(define LOF1 '())
(define LOF2 (list F1))
(define LOF3 (list F1 F2))

(define-struct water (enable? timer))
; A Water is a structure:
;   (make-water Boolean Integer)
; interpretation (make-water e? t) is the status of water control:
;  - e? is the enable of water; and
;  - t is the timer, WATER-LOAD to 0 for drop and 10 to 0 for recharge
(define W1 (make-water #true 0))
(define W2 (make-water #true 3))
(define W3 (make-water #true WATER-LOAD))
(define W4 (make-water #false 0))
(define W5 (make-water #false 5))
(define W6 (make-water #false 10))

(define-struct fire-fighting (fires water time ctr-x))
; A Fire-Fighting is a structure:
;   (make-fire-fighting ListOfFire Water Integer[0, TIMEOUT) Integer[0, WIDTH.V4))
; interpretation (make-fire-fighting lof w t cx) is an instance of the game where:
;  - lof is the references to fires;
;  - w is a Water reference;
;  - t is the time counter, between 0 and TIMEOUT; and
;  - cx is the current x position, between 0 and WIDTH.V4
(define FF (make-fire-fighting LOF1 W1 TIMEOUT CENTER-X))


;; =================
;; Functions:

; Fire-Fighting -> World
; starts a world with (ff-main FF)
(define (ff-main ff)
  (big-bang (create-fires ff)
            (on-tick   ff-tock)
            (on-draw   ff-render)
            (on-key    ff-control)
            (stop-when ff-game-over? ff-render-gamer-over)))

; Fire-Fighting Integer -> Fire-Fighting
; creates fires on the game
(check-random (create-fires FF)
              (make-fire-fighting
               (list (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE))))
                     (make-posn (random WIDTH.V4) (random (- HEIGHT.V4 (image-height AIRPLANE)))))
               W1 TIMEOUT CENTER-X))

(define (create-fires ff)
  (cond [(= (length (fire-fighting-fires ff)) FIRES) ff]
        [else
         (create-fires (make-fire-fighting
                        (append (fire-fighting-fires ff)
                                (list (make-posn (random WIDTH.V4)
                                                 (random (- HEIGHT.V4 (image-height AIRPLANE))))))
                        W1 TIMEOUT CENTER-X))]))

; Fire-Fighting -> Fire-Fighting
; updates the status of the game
(check-expect (ff-tock FF)
              (make-fire-fighting LOF1 W1 (sub1 TIMEOUT) CENTER-X))
(check-expect (ff-tock (make-fire-fighting LOF2 W1 TIMEOUT CENTER-X))
              (make-fire-fighting (list (make-posn 10 21)) W1 (sub1 TIMEOUT) CENTER-X))

(define (ff-tock ff)
  (check-collision
   (make-fire-fighting
    (move-fires (fire-fighting-fires ff))
    (if (= (remainder (fire-fighting-time ff) TOCK-STEP) 0)
        (status-water (fire-fighting-water ff))
        (fire-fighting-water ff))
    (sub1 (fire-fighting-time ff))
    (fire-fighting-ctr-x ff))))

; Fire-Fighting -> Fire-Fighting
; checks the collision between fire and water
(check-expect (check-collision (make-fire-fighting LOF2 W1 TIMEOUT CENTER-X))
              (make-fire-fighting LOF2 W1 TIMEOUT CENTER-X))

(check-expect (check-collision (make-fire-fighting (list (make-posn 10 519)) W2 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 10 519)) W2 TIMEOUT 10))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 520)) W2 TIMEOUT 10))
              (make-fire-fighting '() W2 TIMEOUT 10))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 520)) W2 TIMEOUT 20))
              (make-fire-fighting (list (make-posn 10 520)) W2 TIMEOUT 20))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 520)) W6 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 10 520)) W6 TIMEOUT 10))

(check-expect (check-collision (make-fire-fighting (list (make-posn 10 570)) W2 TIMEOUT 10))
              (make-fire-fighting '() W2 TIMEOUT 10))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 570)) W2 TIMEOUT 20))
              (make-fire-fighting (list (make-posn 10 570)) W2 TIMEOUT 20))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 570)) W6 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 10 570)) W6 TIMEOUT 10))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 571)) W2 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 10 571)) W2 TIMEOUT 10))

(check-expect (check-collision (make-fire-fighting (list (make-posn 10 520) (make-posn 20 520)) W2 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 20 520)) W2 TIMEOUT 10))
(check-expect (check-collision (make-fire-fighting (list (make-posn 10 570) (make-posn 20 570)) W2 TIMEOUT 10))
              (make-fire-fighting (list (make-posn 20 570)) W2 TIMEOUT 10))

(define (check-collision ff)
  (make-fire-fighting
   (cond [(and (water-enable? (fire-fighting-water ff))
               (> (water-timer (fire-fighting-water ff)) 0))
          (remove-in-x (fire-fighting-fires ff) (fire-fighting-ctr-x ff))]
         [else (fire-fighting-fires ff)])
   (fire-fighting-water ff)
   (fire-fighting-time ff)
   (fire-fighting-ctr-x ff)))

; ListOfFire Integer -> ListOfFire
; removes the fires at position:
;  - y: between 520 and 570; and
;  - x: same
(check-expect (remove-in-x '() 0) '())
(check-expect (remove-in-x (list (make-posn 10 520)) 10) '())
(check-expect (remove-in-x (list (make-posn 10 520)) 11) (list (make-posn 10 520)))
(check-expect (remove-in-x (list (make-posn 10 520) (make-posn 20 520)) 10) (list (make-posn 20 520)))
(check-expect (remove-in-x (list (make-posn 10 570)) 10) '())
(check-expect (remove-in-x (list (make-posn 10 570)) 11) (list (make-posn 10 570)))
(check-expect (remove-in-x (list (make-posn 10 570) (make-posn 20 570)) 10) (list (make-posn 20 570)))
(check-expect (remove-in-x (list (make-posn 10 519)) 10) (list (make-posn 10 519)))
(check-expect (remove-in-x (list (make-posn 10 571)) 10) (list (make-posn 10 571)))

(define (remove-in-x lof x)
  (cond [(empty? lof) '()]
        [(and (= (posn-x (first lof)) x)
              (<= 520 (posn-y (first lof)) 570))
         (remove-in-x (rest lof) x)]
        [else (cons (first lof)
                    (remove-in-x (rest lof) x))]))

; ListOfFire -> ListOfFire
; increments the height by 1 in list or reset when equal to HEIGHT.V4
(check-expect (move-fires LOF1) LOF1)
(check-expect (move-fires LOF2) (list (make-posn 10 21)))
(check-expect (move-fires LOF3) (list (make-posn 10 21) (make-posn 50 51)))
(check-expect (move-fires (list (make-posn 10 HEIGHT.V4)))
              (list (make-posn 10 0)))
(check-expect (move-fires (list (make-posn 10 HEIGHT.V4) (make-posn 10 21)))
              (list (make-posn 10 0) (make-posn 10 22)))

(define (move-fires lof)
  (cond [(empty? lof) '()]
        [(= (posn-y (first lof)) HEIGHT.V4)
         (cons (make-posn (posn-x (first lof)) 0)
               (move-fires (rest lof)))]
        [else (cons (make-posn (posn-x (first lof))
                               (add1 (posn-y (first lof))))
                    (move-fires (rest lof)))]))

; Water -> Water
; uptades the status water
(check-expect (status-water W1) (make-water #true 0))
(check-expect (status-water W2) (make-water #true 2))
(check-expect (status-water W3) (make-water #true (sub1 WATER-LOAD)))
(check-expect (status-water W4) W1)
(check-expect (status-water W5) (make-water #false 4))
(check-expect (status-water W6) (make-water #false 9))
(check-expect (status-water (make-water #true 1)) W6)

(define (status-water w)
  (cond [(and (water-enable? w)
              (= (water-timer w) 0)) w]
        [(and (water-enable? w)
              (= (water-timer w) 1)) (make-water #false 10)]
        [(and (not (water-enable? w))
              (= (water-timer w) 0)) (make-water #true 0)]
        [else (make-water (water-enable? w) (sub1 (water-timer w)))]))

; Fire-Fighting -> Image
; renders the given game state on top of MTS
(define (ff-render ff)
  (place-image
   AIRPLANE
   (fire-fighting-ctr-x ff) CTR-Y
   (render-water ff
                 (render-fires (fire-fighting-fires ff)
                               (render-time (fire-fighting-time ff)
                                            (place-image (rectangle WIDTH.V4 1 "solid" "blue")
                                                         CENTER-X 520
                                                         (place-image (rectangle WIDTH.V4 1 "solid" "red")
                                                                      CENTER-X 570 MTS)))))))

; ListOfFire Image -> Image
; renders the fires on top of image
(define (render-fires lof i)
  (cond [(empty? lof) i]
        [else
         (place-image FIRE
                      (posn-x (first lof)) (posn-y (first lof))
                      (render-fires (rest lof) i))]))

; Fire-Fighting Image -> Image
; renders the water on top of image
(define (render-water ff i)
  (cond [(and (water-enable? (fire-fighting-water ff))
              (> (water-timer (fire-fighting-water ff)) 0))
         (place-image WATER (fire-fighting-ctr-x ff) (- CTR-Y (image-height AIRPLANE)) i)]
        [(not (water-enable? (fire-fighting-water ff)))
         (place-image (text "X" 20 "red") (fire-fighting-ctr-x ff) (- CTR-Y (image-height AIRPLANE) -30) i)]
        [else i]))

; Integer Image -> Image
; renders the time on top of image
(define (render-time t i)
  (place-image (text (number->string (inexact->exact (floor (/ t TOCK-STEP)))) 20 "black") CENTER-X 20 i))

; Fire-Fighting KeyEvent -> Fire-Fighting
; handles the main events:
;  - pressing the left arrow ensures that the airplane moves left;
;  - pressing the right arrow ensures that the airplane moves right;
;  - pressing the shift ensures that the airplane moves left with precision;
;  - pressing the ff-control ensures that the airplane moves right with precision; and
;  - pressing the space bar ensures that the airplane releases of water loads
(check-expect (ff-control FF "left")
              (make-fire-fighting LOF1 W1 TIMEOUT (- CENTER-X STEP-X)))
(check-expect (ff-control FF "right")
              (make-fire-fighting LOF1 W1 TIMEOUT (+ CENTER-X STEP-X)))
(check-expect (ff-control FF "shift")
              (make-fire-fighting LOF1 W1 TIMEOUT (sub1 CENTER-X)))
(check-expect (ff-control FF "control")
              (make-fire-fighting LOF1 W1 TIMEOUT (add1 CENTER-X)))
(check-expect (ff-control FF " ")
              (make-fire-fighting LOF1 W3 TIMEOUT CENTER-X))
(check-expect (ff-control (make-fire-fighting LOF1 W2 TIMEOUT CENTER-X) " ")
              (make-fire-fighting LOF1 W2 TIMEOUT CENTER-X))
(check-expect (ff-control FF "a")
              (make-fire-fighting LOF1 W1 TIMEOUT CENTER-X))

(define (ff-control ff ke)
  (make-fire-fighting
   (fire-fighting-fires ff)
   (cond  [(and (key=? ke " ")
                (water-enable? (fire-fighting-water ff))
                (= (water-timer (fire-fighting-water ff)) 0)) W3]
          [else (fire-fighting-water ff)])
   (fire-fighting-time ff)
   (+ (fire-fighting-ctr-x ff)
      (cond [(key=? ke "left") (- STEP-X)]
            [(key=? ke "right") STEP-X]
            [(key=? ke "shift") -1]
            [(key=? ke "control") 1]
            [else 0]))))

; Fire-Fighting -> Boolean
; returns true when the game stop;
; the game stops if extinguish all FIRES before the TIMEOUT
(check-expect (ff-game-over? (make-fire-fighting LOF2 W1 TIMEOUT CENTER-X)) #false)
(check-expect (ff-game-over? FF) #true)
(check-expect (ff-game-over? (make-fire-fighting LOF1 W1 0 CENTER-X)) #true)

(define (ff-game-over? ff)
  (or (= (length (fire-fighting-fires ff)) 0)
      (= (fire-fighting-time ff) 0)))

; Fire-Fighting -> Image
; renders the game over message on the last ff-render
(define (ff-render-gamer-over ff)
  (place-image (text (if (= (fire-fighting-time ff) 0)
                         "Game Over!"
                         "You Win!")
                     32 "black")
               CENTER-X CENTER-Y (ff-render ff)))



;; 12.8 - Finite State Machines

;; Exercise 226
;; Exercise 227
;; Exercise 228
;; Exercise 229
;; Exercise 230


;; =================
;; Data definitions:

; A Color is one of:
; - "white"
; - "yellow"
; - "orange"
; - "green"
; - "red"
; - "blue"
; - "black"

; An FSM is one of:
;  - '()
;  - (cons Transition FSM)

(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)

; FSM-State is a Color.

; interpretation An FSM represents the transitions that a
; finite state machine can take from one state to another
; in reaction to keystrokes
(define fsm-traffic (list (make-transition "red"    "green")
                          (make-transition "green"  "yellow")
                          (make-transition "yellow" "red")))
(define fsm-bw-machine (list (make-transition "black" "white")
                             (make-transition "white" "black")))

; A SimulationState.v1 is an FSM-State.

(define-struct fs [fsm current])
; A SimulationState.v2 is a structure:
;   (make-fs FSM FSM-State)

(define-struct ktransition [current key next])
; A Transition.v2 is a structure:
;   (make-ktransition FSM-State KeyEvent FSM-State)

; ExpectsToSee is one of:
; - "start, expect an 'a'"
; - "expect 'b', 'c', or 'd'"

(define fsm-expectstosee (list (make-ktransition "start"  "a" "expect")
                               (make-ktransition "expect" "b" "expect")
                               (make-ktransition "expect" "c" "expect")
                               (make-ktransition "expect" "d" "expect")))

(define-struct fsm [initial transitions final])
(define-struct transition.v3 [current key next])
; An FSM.v2 is a structure:
;   (make-fsm FSM-State LOT FSM-State)
; A LOT is one of:
; - '()
; - (cons Transition.v3 LOT)
; A Transition.v3 is a structure:
;   (make-transition.v3 FSM-State KeyEvent FSM-State)

; ExpectsToSee is one of:
; - "start, expect an 'a'"
; - "expect 'b', 'c', or 'd'"
; - "finished"
; - "error, illegal key"

(define fsm-expectstosee.v3 (list (make-transition.v3 "start"    "a" "expect")
                                  (make-transition.v3 "start"    "b" "error")
                                  (make-transition.v3 "expect"   "b" "expect")
                                  (make-transition.v3 "expect"   "c" "expect")
                                  (make-transition.v3 "expect"   "d" "finished")
                                  (make-transition.v3 "expect"   "e" "error")
                                  (make-transition.v3 "finished" "e" "finished")
                                  (make-transition.v3 "error"    "e" "error")))


;; =================
;; Functions:

; FSM -> SimulationState.v1
; match the keys pressed with the given FSM
(define initial-state '())

(define (simulate.v1 fsm0)
  (big-bang initial-state
            [to-draw render-state.v1]
            [on-key find-next-state.v1]))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v2 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
            [to-draw state-as-colored-square]
            [on-key  find-next-state]))

; FSM FSM-State -> SimulationState.v2
; match the keys pressed with the given FSM
(define (simulate.v3 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
            [to-draw state-as-colored-square.v2]
            [on-key  find-next-state.v2]))

; FSM.v2 -> SimulationState
; match the keys pressed with the given FSM
(define (fsm-simulate a-fsm)
  (big-bang a-fsm
            [to-draw   state-as-colored-square.v3]
            [on-key    find-next-state.v3]
            [stop-when reach-final-state? state-as-colored-square.v3]))

; SimulationState.v1 -> Image
; renders a world state as an image
(define (render-state.v1 s)
  empty-image)

; SimulationState.v1 KeyEvent -> SimulationState.v1
; finds the next state from ke and cs
(define (find-next-state.v1 cs ke)
   cs)

; SimulationState.v2 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))

(define (state-as-colored-square an-fsm)
  (square 100 "solid" (fs-current an-fsm)))

; SimulationState.v2 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square.v2 (make-fs fsm-expectstosee "start"))
              (square 100 "solid" "white"))
(check-expect (state-as-colored-square.v2 (make-fs fsm-expectstosee "expect"))
              (square 100 "solid" "yellow"))

(define (state-as-colored-square.v2 a-fs)
  (square 100 "solid"
          (cond [(state=? "start" (fs-current a-fs)) "white"]
                [else "yellow"])))

; SimulationState.v2 -> Image
; renders current world state as a colored square
(check-expect (state-as-colored-square.v3 (make-fsm "start" fsm-expectstosee "finished"))
              (square 100 "solid" "white"))
(check-expect (state-as-colored-square.v3 (make-fsm "expect" fsm-expectstosee "finished"))
              (square 100 "solid" "yellow"))
(check-expect (state-as-colored-square.v3 (make-fsm "finished" fsm-expectstosee "finished"))
              (square 100 "solid" "green"))
(check-expect (state-as-colored-square.v3 (make-fsm "error" fsm-expectstosee "finished"))
              (square 100 "solid" "red"))

(define (state-as-colored-square.v3 a-fs)
  (square 100 "solid" (cond [(state=? "start"    (fsm-initial a-fs)) "white"]
                            [(state=? "expect"   (fsm-initial a-fs)) "yellow"]
                            [(state=? "finished" (fsm-initial a-fs)) "green"]
                            [else "red"])))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect (find-next-state (make-fs fsm-traffic "red") "n")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "red") "a")
              (make-fs fsm-traffic "green"))
(check-expect (find-next-state (make-fs fsm-traffic "green") "q")
              (make-fs fsm-traffic "yellow"))

(define (find-next-state an-fsm current)
  (make-fs (fs-fsm an-fsm)
           (find (fs-fsm an-fsm) (fs-current an-fsm))))

; FSM FSM-State -> FSM-State
; finds the state representing current in transitions
; and retrieves the next field
(check-expect (find fsm-traffic "red")   "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error  (find fsm-traffic "black") "not found: black")

(define (find transitions current)
  (cond [(empty? transitions)
         (error "not found: " current)]
        [(state=? (transition-current (first transitions)) current)
         (transition-next (first transitions))]
        [else (find (rest transitions) current)]))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect (find-next-state.v2 (make-fs fsm-expectstosee "start")  "b")
              (make-fs fsm-expectstosee "start"))
(check-expect (find-next-state.v2 (make-fs fsm-expectstosee "start")  "a")
              (make-fs fsm-expectstosee "expect"))
(check-expect (find-next-state.v2 (make-fs fsm-expectstosee "expect") "b")
              (make-fs fsm-expectstosee "expect"))
(check-expect (find-next-state.v2 (make-fs fsm-expectstosee "expect") "c")
              (make-fs fsm-expectstosee "expect"))
(check-expect (find-next-state.v2 (make-fs fsm-expectstosee "expect") "d")
              (make-fs fsm-expectstosee "expect"))

(define (find-next-state.v2 a-fs ke)
  (make-fs (fs-fsm a-fs)
           (find.v2 (fs-fsm a-fs) (fs-current a-fs) ke)))

; FSM FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieve the next field
(check-expect (find.v2 fsm-expectstosee "start"  "a") "expect")
(check-expect (find.v2 fsm-expectstosee "start"  "b") "start")
(check-expect (find.v2 fsm-expectstosee "expect" "b") "expect")
(check-expect (find.v2 fsm-expectstosee "expect" "c") "expect")
(check-expect (find.v2 fsm-expectstosee "expect" "d") "expect")

(define (find.v2 transitions current key)
  (cond [(empty? transitions) current]
        [(and (state=? (ktransition-current (first transitions)) current)
              (key=?   (ktransition-key     (first transitions)) key))
         (ktransition-next (first transitions))]
        [else
         (find.v2 (rest transitions) current key)]))

; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect (find-next-state.v3 (make-fsm "start" fsm-expectstosee.v3 "finished") "a")
              (make-fsm "expect" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "start" fsm-expectstosee.v3 "finished") "b")
              (make-fsm "error" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "expect" fsm-expectstosee.v3 "finished") "b")
              (make-fsm "expect" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "expect" fsm-expectstosee.v3 "finished") "c")
              (make-fsm "expect" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "expect" fsm-expectstosee.v3 "finished") "d")
              (make-fsm "finished" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "expect" fsm-expectstosee.v3 "finished") "e")
              (make-fsm "error" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "finished" fsm-expectstosee.v3 "finished") "e")
              (make-fsm "finished" fsm-expectstosee.v3 "finished"))
(check-expect (find-next-state.v3 (make-fsm "error" fsm-expectstosee.v3 "finished") "e")
              (make-fsm "error" fsm-expectstosee.v3 "finished"))

(define (find-next-state.v3 a-fs ke)
  (make-fsm (find.v3 (fsm-transitions a-fs) (fsm-initial a-fs) ke)
            (fsm-transitions a-fs)
            (fsm-final a-fs)))

; FSM.v2 FSM-State KeyEvent -> FSM-State
; finds the state representing current in transitions
; and retrieve the next field
(check-expect (find.v3 fsm-expectstosee.v3 "start"    "a") "expect")
(check-expect (find.v3 fsm-expectstosee.v3 "start"    "b") "error")
(check-expect (find.v3 fsm-expectstosee.v3 "expect"   "b") "expect")
(check-expect (find.v3 fsm-expectstosee.v3 "expect"   "c") "expect")
(check-expect (find.v3 fsm-expectstosee.v3 "expect"   "d") "finished")
(check-expect (find.v3 fsm-expectstosee.v3 "expect"   "e") "error")
(check-expect (find.v3 fsm-expectstosee.v3 "finished" "e") "finished")
(check-expect (find.v3 fsm-expectstosee.v3 "error"    "e") "error")

(define (find.v3 transitions current key)
  (cond [(empty? transitions) "error"]
        [(and (state=? (transition.v3-current (first transitions)) current)
              (key=?   (transition.v3-key     (first transitions)) key))
         (transition.v3-next (first transitions))]
        [else (find.v3 (rest transitions) current key)]))

; SimulationState.v2 -> Boolean
; stops when FSM-State is "finished" or "error"
(check-expect (reach-final-state? (make-fsm "start"    fsm-expectstosee.v3 "finished")) #false)
(check-expect (reach-final-state? (make-fsm "expect"   fsm-expectstosee.v3 "finished")) #false)
(check-expect (reach-final-state? (make-fsm "finished" fsm-expectstosee.v3 "finished")) #true)
(check-expect (reach-final-state? (make-fsm "error"    fsm-expectstosee.v3 "finished")) #true)

(define (reach-final-state? a-fs)
  (or (state=? "finished" (fsm-initial a-fs))
      (state=? "error"    (fsm-initial a-fs))))

; FSM-State FSM-State -> Boolean
; checks an equality predicate for states
(check-expect (state=? "white" "white")  #true)
(check-expect (state=? "white" "yellow") #false)
(check-expect (state=? "start" "start")  #true)
(check-expect (state=? "start" "expect") #false)

(define (state=? s1 s2)
  (string=? s1 s2))
