#lang sicp
(define (square x) (* x x))

(define (square-list3 items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things) (cons (square (car things)) answer))
        )
    )
    (iter items nil)
)

(square-list3 (list 1 2 3 4))