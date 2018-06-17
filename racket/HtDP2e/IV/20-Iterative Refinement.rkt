;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |20-Iterative Refinement|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; 20-Iterative Refinement.rkt
;; IV - Intertwined Data
;; 20 - Iterative Refinement



;; 20.1 - Data Analysis

;; Exercise 329

; How many times does a file name read! occur in the directory tree TS?
; Occur 2 times.

; Can you describe the path from the root directory to the occurrences?
; Yes.
; TS (DIR) -> read! (10)
; TS (DIR) -> Libs (DIR) -> Docs (DIR) -> read! (19)

; What is the total size of all the files in the tree?
; Total of 207.

; What is the total size of the directory if each directory node has size 1?
; Total of 5.

; How many levels of directories does it contain?
; It has 3 levels.



;; 20.2 - Refining Data Definitions


;; =================
;; Data definitions:

; A Dir.v1 (short for directory) is one of:
; - '()
; - (cons File.v1 Dir.v1)
; - (cons Dir.v1  Dir.v1)

; A File.v1 is a String.

;; Exercise 330

(define D-CODE '("hang" "draw"))
(define D-DOCS '("read!"))
(define D-LIBS `(,D-CODE ,D-DOCS))
(define D-TEXT '("part1" "part2" "part3"))
(define D-TS   `(,D-TEXT "read!" ,D-LIBS))

;; Exercise 331


;; =================
;; Functions:

; Dir.v1 -> Natural
; determines how many files a given Dir.v1 contains
(check-expect (how-many '())    0)
(check-expect (how-many D-CODE) 2)
(check-expect (how-many D-LIBS) 3)
(check-expect (how-many D-TS)   7)

(define (how-many d)
  (foldl (lambda (d acc)
           (+ acc
              (if (string? d) 1 (how-many d))))
         0 d))


;; =================
;; Data definitions:

(define-struct dir.v2 [name content])
; A Dir.v2 is a structure:
;   (make-dir.v2 String LOFD)

; An LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2  LOFD)

; A File.v2 is a String.

;; Exercise 332

(define D-CODE.V2 (make-dir.v2 "Code" '("hang" "draw")))
(define D-DOCS.V2 (make-dir.v2 "Docs" '("read!")))
(define D-LIBS.V2 (make-dir.v2 "Libs" `(,D-CODE.V2 ,D-DOCS.V2)))
(define D-TEXT.V2 (make-dir.v2 "Text" '("part1" "part2" "part3")))
(define D-TS.V2   (make-dir.v2 "TS"   `(,D-TEXT.V2 "read!" ,D-LIBS.V2)))

;; Exercise 333

#;
(define (fn-for-dir.v2 d)
  (... (dir.v2-name d)
       (fn-for-lofd (dir.v2-content d))))

#;
(define (fn-for-lofd lofd)
  (cond [(empty? lofd) ...]
        [(string? (first lofd))
         (... (first lofd)
              (fn-for-lofd (rest lofd)))]
        [else
         (... (fn-for-dir  (first lofd))
              (fn-for-lofd (rest  lofd)))]))


;; =================
;; Functions:

; Dir.v2 -> Natural
; determines how many files a given Dir.v2 contains
(check-expect (how-many.v2 (make-dir.v2 "root" '())) 0)
(check-expect (how-many.v2 D-CODE.V2) 2)
(check-expect (how-many.v2 D-LIBS.V2) 3)
(check-expect (how-many.v2 D-TS.V2)   7)

(define (how-many.v2 d)
  (foldl (lambda (d acc)
           (+ acc
              (if (string? d) 1 (how-many.v2 d))))
         0 (dir.v2-content d)))

;; Exercise 334


;; =================
;; Data definitions:

(define-struct dir.v2b (name content size readability))
; A Dir.v2b is a structure:
;   (make-dir.v2b String LOFD N Boolean)

; A LOFD (short for list of files and directories) is one of:
; - '()
; - (cons File.v2 LOFD)
; - (cons Dir.v2b LOFD)

; A File.v2 is a String.

(define-struct file.v3 [name size content])
; A File.v3 is a structure:
;   (make-file.v3 String N String)

(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure:
;   (make-dir.v3 String Dir* File*)

; A Dir* is one of:
; - '()
; - (cons Dir.v3 Dir*)

; A File* is one of:
; - '()
; - (cons File.v3 File*)

;; Exercise 335

(define F7 (make-file.v3 "read!" 19 ""))
(define D-DOCS.V3 (make-dir.v3 "Docs" '() `(,F7)))
(define F5 (make-file.v3 "hang" 8 ""))
(define F6 (make-file.v3 "draw" 2 ""))
(define D-CODE.V3 (make-dir.v3 "Code" '() `(,F5 ,F6)))
(define D-LIBS.V3 (make-dir.v3 "Libs" `(,D-CODE.V3 ,D-DOCS.V3) '()))
(define F2 (make-file.v3 "part1" 99 ""))
(define F3 (make-file.v3 "part2" 52 ""))
(define F4 (make-file.v3 "part3" 17 ""))
(define D-TEXT.V3 (make-dir.v3 "Text" '() `(,F2 ,F3 ,F4)))
(define F1 (make-file.v3 "read!" 10 ""))
(define D-TS.V3 (make-dir.v3 "TS" `(,D-TEXT.V3 ,D-LIBS.V3) `(,F1)))

;; Exercise 336

#;
(define (fn-for-dir.v3 d0)
  (local ((define (fn-for-dir.v3 d)
            (... (dir.v3-name d)
                 (fn-for-lod (dir.v3-dirs  d))
                 (fn-for-lof (dir.v3-files d))))

          (define (fn-for-lod lod)
            (cond [(empty? lod) ...]
                  [else
                   (... (fn-for-dir.v3 (first lod))
                        (fn-for-lod    (rest  lod)))]))

          (define (fn-for-lof lof)
            (cond [(empty? lof) ...]
                  [else
                   (... (fn-for-file.v3 (first lof))
                        (fn-for-lof     (rest  lof)))]))

          (define (fn-for-file.v3 f)
            (... (file.v3-name f)
                 (file.v3-size f)
                 (file.v3-content f))))

    (fn-for-dir.v3 d0)))


;; =================
;; Functions:

; Dir.v3 -> Natural
; determines how many files a given Dir.v3 contains
(check-expect (how-many.v3 (make-dir.v3 "root" '() '())) 0)
(check-expect (how-many.v3 D-CODE.V3) 2)
(check-expect (how-many.v3 D-LIBS.V3) 3)
(check-expect (how-many.v3 D-TS.V3)   7)

(define (how-many.v3 d)
  (+ (foldl (lambda (d acc)
              (+ (how-many.v3 d) acc))
            0 (dir.v3-dirs d))
     (foldl (lambda (f acc)
              (add1 acc))
            0 (dir.v3-files d))))

;; Exercise 337


;; =================
;; Data definitions:

; A Dir.v3 is a structure:
;   (make-dir.v3 String [List-of Dir.v3] [List-of File.v3])

; A [List-of ITEM] is one of:
; - '()
; - (cons ITEM [List-of ITEM])


;; =================
;; Functions:

; Dir.v3 -> Natural
; determines how many files a given Dir.v3 contains
(check-expect (how-many.v4 (make-dir.v3 "root" '() '())) 0)
(check-expect (how-many.v4 D-CODE.V3) 2)
(check-expect (how-many.v4 D-LIBS.V3) 3)
(check-expect (how-many.v4 D-TS.V3)   7)

(define (how-many.v4 d)
  (+ (foldr (lambda (d acc)
              (+ (how-many.v4 d) acc))
            0 (dir.v3-dirs d))
     (length (dir.v3-files d))))



;; 20.3 - Refining Functions

(require htdp/dir)


;; (define O (create-dir "/Users/..."))     ; on OS X
(define L (create-dir "/var/log/"))      ; on Linux
;; (define W (create-dir "C:\\Users\\...")) ; on Windows

;; Exercise 338


;; =================
;; Functions:

; Dir -> Natural
; determines how many files a given Dir contains
(define (how-many.v5 d)
  (+ (foldl (lambda (d acc)
              (+ (how-many.v5 d) acc))
            0 (dir-dirs d))
     (foldl (lambda (f acc)
              (add1 acc))
            0 (dir-files d))))

(how-many.v5 L)

;; Exercise 339


;; =================
;; Functions:

; Dir String -> Boolean
; determines whether or not a file with this name occurs in the directory tree
(define (find? d fn)
  (or (foldl (lambda (d acc)
               (or (find? d fn) acc))
             #false (dir-dirs d))
      (foldl (lambda (f acc)
               (or (string=? (file-name f) fn) acc))
             #false (dir-files d))))

(find? L "log.log")
(find? L "vboxadd-setup.log")

;; Exercise 340


;; =================
;; Functions:

; Dir -> [List-of String]
; lists the names of all files and directories in a given Dir
(define (ls d)
  (append (foldl (lambda (d acc)
                   (append (ls d) acc))
                 '() (dir-dirs d))
          (foldl (lambda (f acc)
                   (cons (file-name f) acc))
                 '() (dir-files d))))

(ls L)

;; Exercise 341


;; =================
;; Functions:

; Dir -> Natural
; computes the total size of all the files in the entire directory tree
(define (du d)
  (+ (foldl (lambda (d acc)
              (+ 1 (du d) acc))
            0 (dir-dirs d))
     (foldl (lambda (f acc)
              (+ (file-size f) acc))
            0 (dir-files d))))

(du L)


;; =================
;; Data definitions:

; A Path is [List-of String].
; interpretation directions into a directory tree

;; Exercise 342


;; =================
;; Functions:

; Dir String -> Path or False
; produces a path to a file with name f; otherwise it produces #false
;; TODO refatorar com abstração
(define (find d f)
  (local ((define (fn-for-dir d)
            (if (string=? (fn-for-lof (dir-files d)) "")
                (fn-for-lod (dir-dirs d))
                (cons (dir-name d)
                      (cons (fn-for-lof (dir-files d))
                            '()))))

          (define (fn-for-lod lod)
            (cond [(empty? lod) '()]
                  [else
                   (if (find? (first lod) f)
                       (fn-for-dir (first lod))
                       (fn-for-lod (rest  lod)))]))

          (define (fn-for-lof lof)
            (foldr (lambda (file acc)
                     (if (string=? (file-name file) f)
                         (file-name file)
                         acc))
                   "" lof)))

    (if (find? d f)
        (fn-for-dir d)
        #false)))

(find L "log.log")
(find L "vboxadd-setup.log")
(find L "system.journal")
(find L "new-file")

; Dir String -> [List-of Path] or False
; produces a list of path to a file with name f; otherwise it produces #false
;; FIXME remover os valores duplicados
(define (find-all d f)
  (local ((define (all-valid-paths d)
            (cons (find d f)
                  (foldr (lambda (d acc) (append (all-valid-paths d) acc)) '() (dir-dirs d)))))

    (if (find? d f)
        (all-valid-paths d)
        #false)))

(find-all L "log.log")
(find-all L "vboxadd-setup.log")
(find-all L "system.journal")
(find-all L "new-file")

;; Exercise 343


;; =================
;; Functions:

; Dir -> [List-of Path]
; lists the paths to all files contained in a given Dir
(define (ls-R d0)
  (local ((define (ls-R d)
            (append (foldr (lambda (d acc)
                             (append (ls-R d) acc))
                           '() (dir-dirs d))
                    (foldr (lambda (f acc)
                             (cons (find d0 (file-name f)) acc))
                           '() (dir-files d)))))
    (ls-R d0)))

(ls-R L)

;; Exercise 344


;; =================
;; Functions:

; Dir String -> [List-of Path] or False
; produces the list of all paths that lead to f in d; otherwise it produces #false
(define (find-all.v2 d f)
  (local ((define result (filter (lambda (p) (member f p)) (ls-R d))))
    (if (empty? result)
        #false
        result)))

(find-all.v2 L "log.log")
(find-all.v2 L "vboxadd-setup.log")
(find-all.v2 L "system.journal")
(find-all.v2 L "new-file")
