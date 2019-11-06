#lang sicp

(define (cons x y)
    (lambda (m) (m x y))
)

(define (car z)
    (z (lambda (p q) p))
)

(define (cdr z)
    (z (lambda (p q) q))
)

(car (cons 33 44))
(cdr (cons 33 44))