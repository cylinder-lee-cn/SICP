#lang sicp
(define (make-accumulator x)
    (lambda (y)
        (begin (set! x (+ x y)))
        x
    )
)

(define A (make-accumulator 5))

(A 10)
(A 10)
(A 4)

(define B (make-accumulator 7))
(B 9)
(B 17)
