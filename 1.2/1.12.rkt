#lang racket

; "Classic" solution

(define (pt-value row col)
  (if (or (= col 1) (= col row))
      1
      (+ (pt-value (- row 1) (- col 1)) (pt-value (- row 1) col))))


; > (pt-value 5 3)
; 6
; > (pt-value 10 7)
; 84
; > (pt-value 1 1)
; 1
; > (pt-value 8 3)
; 21


; Threading + Algorithms solution

(require algorithms)
(require threading)

(define (pt-next-row row)
  (~> (sliding row 2)
      (map (λ (x) (foldl + 0 x)) _)
      (append '(1) _ '(1))))

(define (pt-iter acc n)
  (if (= 0 n)
      (reverse acc)
      (pt-iter (cons (pt-next-row (first acc)) acc) (- n 1))))

(define (pascal-triangle n)
  (if (= 1 n)
      '(1)
      (pt-iter '((1 1) (1)) (- n 2))))

; > (pascal-triangle 10)
; '((1)
;   (1 1)
;   (1 2 1)
;   (1 3 3 1)
;   (1 4 6 4 1)
;   (1 5 10 10 5 1)
;   (1 6 15 20 15 6 1)
;   (1 7 21 35 35 21 7 1)
;   (1 8 28 56 70 56 28 8 1)
;   (1 9 36 84 126 126 84 36 9 1))
