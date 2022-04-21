#lang racket

(require threading)

(define (smallest-divisor n)
  (define (iter i)
    (cond ((= (remainder n i) 0) i)
          ((> (* i i) n) n)
          (else (iter (+ i 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (timed-prime-test n)
  (start-prime-test n (current-milliseconds)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (current-milliseconds) start-time) n)
      (display "")))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (display "ms")
  (newline))

(define (search-for-primes start end count)
  (~> (range start (+ end 1))
      (filter (λ (x) (prime? x)) _)
      (take _ count)))


; (timed-prime-test 2147483647)
; 2147483647 *** 1ms
; (timed-prime-test 67280421310721)
; 67280421310721 *** 117ms


(search-for-primes 1 10 7)

; (search-for-primes 1000 10000 5)
; '(1009 1013 1019 1021 1031)
; (search-for-primes 10000 100000 5)
; '(10007 10009 10037 10039 10061)
; (search-for-primes 100000 1000000 5)
; '(100003 100019 100043 100049 100057)
; (search-for-primes 1000000 100000000 5)
; '(1000003 1000033 1000037 1000039 1000081)
; (search-for-primes 100000000 100000000 5)
; '(100000007 100000037 100000039 100000049 100000073)




