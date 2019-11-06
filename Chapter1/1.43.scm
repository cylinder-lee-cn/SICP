#lang sicp

(define (square x)
    (* x x))

(define (compose f g)
    (lambda (x) (f (g x)))
)

(define (repeated g k)
    (if (= k 1)
        g
        (compose g (repeated g (dec k)))
    )
)

((repeated square 2) 5)