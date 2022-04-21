#lang racket

; Recursive solution

(define(f-rec n)
  (if (< n 3) n
      (+ (f-rec (- n 1))
         (* 2 (f-rec (- n 2)))
         (* 3 (f-rec(- n 3))))))

; > (map f-rec (range 10))
; '(0 1 2 4 11 25 59 142 335 796)


; Iterative solution

(define (f-iter n)
  (define (iter a b c count)
    (cond ((= 0 count) a)
          (else (iter b
                      c
                      (+ c
                         (* 2 b)
                         (* 3 a))
                      (- count 1)))))
  (iter 0 1 2 n))

; > (map f-iter (range 10))
; '(0 1 2 4 11 25 59 142 335 796)
