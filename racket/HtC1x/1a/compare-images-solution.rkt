;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname compare-images-solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; compare-images-solution.rkt
;; BSL P5 - Compare Images
;; Write expressions to operate on booleans using primitives.

(require 2htdp/image)


;;
;; PROBLEM:
;;
;; Based on the two constants provided, write three expressions to determine whether: 
;;
;; 1) IMAGE1 is taller than IMAGE2
;; 2) IMAGE1 is narrower than IMAGE2
;; 3) IMAGE1 has both the same width AND height as IMAGE2

(define IMAGE1 (rectangle 10 15 "solid" "red"))
(define IMAGE2 (rectangle 15 10 "solid" "red"))

(> (image-height IMAGE1)
   (image-height IMAGE2))

(< (image-width IMAGE1)
   (image-width IMAGE2))

(and (= (image-width IMAGE1) 
        (image-width IMAGE2)) 
     (= (image-height IMAGE1)
        (image-height IMAGE2)))
