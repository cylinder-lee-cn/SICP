#lang sicp
(define (smallset-divisor n)
    (find-divisor n 2)
)

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))
    )
)

(define (divides? a b)
    (= (remainder b a) 0)
)

(define (square x) (* x x)
)

(define (next-divisor n)
    (if (= n 2)
        3
        (+ n 2)
    )
)

(smallset-divisor 91)