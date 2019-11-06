#lang racket
#|
使用smallest-divisor 找到下列个数的最小因子，199 1999 19999
|#

(define (smallest-divisor n)
    (find-divisor n 2)
)

; (define (find-divisor n test-divisor)
;     (cond ((> (square test-divisor) n) n)
;           ((divisor? test-divisor n) test-divisor)
;           (else (find-divisor n (+ test-divisor 1)))
;     )
; )

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divisor? test-divisor n) test-divisor)
          (else (find-divisor n (next-divisor test-divisor)))
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

(define (next-divisor n)
    (if (= n 2)
        3
        (+ n 2)
    )
)

(define (isprime n m)
    (if (and (prime? n) (> n 1))
        n
        0
    )
)
(provide (all-defined-out))
; (display (smallest-divisor 199))
; (newline)
; (display (smallest-divisor 1999))
; (newline)
; (display (smallest-divisor 19999))

