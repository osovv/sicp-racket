#lang racket

; Recursive "fast" multiplication solution

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult-rec a b)
  (cond ((= b 0) 0)
        ((even? b) (mult-rec (double a) (halve b)))
        (else (+ a (mult-rec a (- b 1))))))

; > (mult-rec 5 5)
; 25
; > (mult-rec 10 15)
; 150
; > (mult-rec 100 100)
; 10000
