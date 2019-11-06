#lang sicp

(define (add-rat x y)
    (make-rat (+ (* (numer x) (demon y))
                 (* (numer y) (demon x)))
              (* (demon x) (demon y))
    )
)


(define (sub-rat x y)
    (make-rat (- (* (numer x) (demon y)
                 (* (numer y) (demon x)))
              )
              (* (demon x) (demon y))
    )
)

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (demon x) (demon y))
    )
)

(define (div-rat x y)
    (make-rat (* (numer x) (demon y))
              (* (demon x) (numer y))
    )
)

(define (equal-rat? x y)
    (= (* (numer x) (demon y))
       (* (demon x) (numer y)))
)

(define (make-rat n d)
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))
    )
    
)
(define (numer x) (car x))

(define (demon x) (cdr x))

(define (print-rat x)
    (newline)
    (display (numer x))
    (display "/")
    (display (demon x))
)

(add-rat (make-rat 1 2) (make-rat 1 4))

(print-rat (make-rat 2 3))