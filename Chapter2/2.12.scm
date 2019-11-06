#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w))
)

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2)
)
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (make-center-percent c p)
    (make-interval (* c (- 1 p)) (* c (+ 1 p)))
)

(define (make-center-percent1 c p)
    (make-center-width c (* c p))
)
;make-center-percent1的效率应该更好一些，只做了一次乘法

(define (percent i)
    (/ (width i) (center i))
)
