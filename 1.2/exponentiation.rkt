#lang racket

; Recursive solution

(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; Iterative solution

(define (expt-iter b n)
  (define (iter acc base count)
    (if (= count 0)
        acc
        (iter (* acc base) base (- count 1))))
  (iter 1 b n))


; Threading + Algorithms solution

(require threading)
(require algorithms)

(define (expt-algo b n)
  (~> (repeat n b)
      (foldl * 1 _)))
