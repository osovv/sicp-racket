#lang racket

; Iterative "fast" multiplication solution

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult-iter a b)
  (define (iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (iter (double a) (halve b) acc))
          (else (iter a (- b 1) (+ acc a)))))
  (iter a b 0))

; > (mult-iter 5 5)
; 25
; > (mult-iter 10 15)
; 150
; > (mult-iter 100 100)
; 10000
