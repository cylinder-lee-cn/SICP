#lang racket
#|
Use put & get must be `racket`
|#

(define nil `())

(define (square x) (* x x))

;; for list
(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence) (filter predicate (cdr sequence)))
          )
          (else (filter predicate (cdr sequence)))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))
    )
)

;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)
(define (=number? exp num) (and (number? exp) (= exp num)))


(provide (all-defined-out))