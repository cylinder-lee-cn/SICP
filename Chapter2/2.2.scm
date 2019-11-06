#lang sicp

(define (avg a b)
    (/ (+ a b) 2.0)
)

(define (make-point x y)
    (cons x y)
)
(define (x-point p)
    (car p)
)
(define (y-point p)
    (cdr p)
)

(define (make-segment s e)
    (cons s e)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (midpoint-segment segment)
    (cons 
        (avg (x-point (start-segment segment)) (x-point (end-segment segment)))
        (avg (y-point (start-segment segment)) (y-point (end-segment segment)))
    )
)

(midpoint-segment (make-segment (make-point 1 2) (make-point 3 4)))

(define (print-point p)
    (newline)
    (display "[")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display "]")
)

(print-point (make-point 4 9))
(print-point (midpoint-segment (make-segment (make-point 1 2) (make-point 3 4))))