#lang racket

(define (square x) (* x x))

; Recursive solution

(define (fast-expt-rec b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-rec b (/ n 2))))
        (else (* b (fast-expt-rec b (- n 1))))))

; > (fast-expt-rec 5 2)
; 25
; > (fast-expt-rec 10 3)
; 1000
; > (fast-expt-rec 5 6)
; 15625


; Iterative solution

(define (fast-expt-iter b n)
  (define (iter squares extra N)
    (cond ((= N 1) (* squares extra))
          ((even? N) (iter (square squares) extra (/ N 2)))
          (else (iter squares (* extra squares) (- N 1)))))
  (iter b 1 n))

; > (fast-expt-iter 5 2)
; 25
; > (fast-expt-iter 10 3)
; 1000
; > (fast-expt-iter 5 6)
; 15625
