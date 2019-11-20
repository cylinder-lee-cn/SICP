#lang sicp

(define (make-segment start-v end-v)
    (list start-v end-v)
)

(define (start-segment seg)
    (car seg)
)

(define (end-segment seg)
    (cadr seg)
)