#lang racket

; Recursive solution

(define (fib-rec n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-rec (- n 1)) (fib-rec (- n 2))))))

; > (map fib-rec (range 20))
; '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)

; Iterative solution

(define (fib-iter n)
  (define (iter a b n max-n)
    (if (> n max-n)
        a
        (iter (+ a b) a (+ n 1) max-n)))
  (iter 0 1 1 n))

; > (map fib-iter (range 20))
; '(0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 610 987 1597 2584 4181)

