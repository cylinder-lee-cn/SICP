#lang sicp
(#%require "1.21.scm")

(define (accumlate1 combiner null-value term a next b filter)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term (filter a b)) result))
        )
    )
    (iter a null-value)
)

(define (sum term a next b filter)
    (accumlate1 + 0 term a next b filter)
)

(define (product term a next b filter)
    (accumlate1 * 1 term a next b filter)
)

(define (f x)
    x
)
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

(define (isgcd? i n)
    (if (and (= 1 (gcd i n)) (< i n))
        i
        1
    )
)
; (sum f 1 inc 10 isprime)
(product f 1 inc 10 isgcd?)
