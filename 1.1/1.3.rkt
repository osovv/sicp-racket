#lang racket

(define (square x) (* x x))

(define
  (sum-of-squares a b)
  (+ (square a) (square b)))


(sum-of-squares 5 5)
                                