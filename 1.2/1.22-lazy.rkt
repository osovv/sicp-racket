#lang lazy

; Lazy + Algorithms solution

; (define (prime? n)
; (if (> n 1)
;     (empty?
;      (filter
;       (λ (x) (eq?
;               (remainder n x)
;               0))
;       (range 2 (- n 1))))
;     #f))

(define (smallest-divisor n)
  (define (iter i)
    (cond ((= (remainder n i) 0) i)
          ((> (* i i) n) n)
          (else (iter (+ i 1)))))
  (iter 2))

(define (prime? n)
  (= (smallest-divisor n) n))

(define (nats start) (cons start (map add1 (nats start))))

(define (search-for-primes-algo start count)
  (begin
    (define nums (filter prime? (nats start)))
    (!! (take count nums))))

(define (search-for-primes-algo-alt-syntax start count)
  (take
   count
   (filter
    prime?
    (nats start))))

; > (search-for-primes-algo 1000 5)
; '(1009 1013 1019 1021 1031)
; > (search-for-primes-algo 10000 5)
; '(10007 10009 10037 10039 10061)
; > (search-for-primes-algo 100000 5)
; '(100003 100019 100043 100049 100057)
; > (search-for-primes-algo 1000000 5)
; '(1000003 1000033 1000037 1000039 1000081)
; > (search-for-primes-algo 100000000 5)
; '(100000007 100000037 100000039 100000049 100000073)


(search-for-primes-algo 1 7)

(search-for-primes-algo 1000 5)
; '(1009 1013 1019 1021 1031)
(search-for-primes-algo 10000 5)
; '(10007 10009 10037 10039 10061)
(search-for-primes-algo 100000 5)
; '(100003 100019 100043 100049 100057)
(search-for-primes-algo 1000000 5)
; '(1000003 1000033 1000037 1000039 1000081)
(search-for-primes-algo 100000000 5)
; '(100000007 100000037 100000039 100000049 100000073)
