#lang sicp
#|
素数
|#

(define (smallest-divisor n)
    (find-divisor n 2)
)

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divisor? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))
    )
)

(define (divisor? a b)
    (= (remainder b a) 0)
)

(define (square x)
    (* x x)
)

(define (prime? n)
    (= n (smallest-divisor n))
)

(prime? 4)
