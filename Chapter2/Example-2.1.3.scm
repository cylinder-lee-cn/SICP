#lang sicp

(define (cons x y)
    (define (dispatch m)
        (cond ((= m 0) x)
              ((= m 1) y)
        (else (error "Argument not 0 or 1 -- CONS" m)))
    )
    dispatch
)
(define (car z) (z 0))
(define (cdr z) (z 1))

(define (cons1 a b)
    (lambda (pick)
        (cond ((= pick 1) a)
              ((= pick 2) b)
        )
    )
)
(define (car1 x) (x 1))
(define (cdr1 x) (x 2))

(cdr (cons 33 44))
(car1 (cons1 37 46))