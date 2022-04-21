#lang racket

; Time helper
(define (measure-time proc)
  (define start-time (current-milliseconds))
  (define result (proc))
  (define end-time (current-milliseconds))
  (display "Result:" result)
  (display "Elapsed time:" (* 1000 (- end-time start-time)))
  )

; Recursive solution

(define (fact-rec n)
  (if (< n 1)
      1
      (* n (fact-rec (- n 1)))))

; > (map fact-rec (range 10))
; '(1 1 2 6 24 120 720 5040 40320 362880)


; Iterative solution

(define (fact-iter n)
  (define (iter acc n)
    (if (= n 0)
        acc
        (iter (* acc n) (- n 1))))
  (iter 1 n))

; > (map fact-iter (range 10))
; '(1 1 2 6 24 120 720 5040 40320 362880)


; Threading + Algorithms solution

(require threading)

(define (fact-algo n)
  (~> (range 1 (+ n 1))
      (foldl * 1 _)))

; > (map fact-algo (range 10))
; '(1 1 2 6 24 120 720 5040 40320 362880)
