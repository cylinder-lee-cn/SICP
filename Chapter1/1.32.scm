#lang sicp

(define (accumlate combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumlate combiner null-value term (next a) next b))
    )
)

(define (accumlate1 combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (combiner (term a) result))
        )
    )
    (iter a null-value)
)

(define (sum term a next b)
    (accumlate1 + 0 term a next b)
)

(define (product term a next b)
    (accumlate1 * 1 term a next b)
)

(define (f x)
    x
)


(sum f 1 inc 100)
(product f 1 inc 10)