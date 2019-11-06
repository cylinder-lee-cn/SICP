#lang sicp

(define dx 0.00001)

(define (square x) (* x x))

(define (smooth f)
    (lambda (x) 
        (/ (+ (f x) (f (- x dx)) (f (+ x dx))) 3)
    )
)

(define (smooth-repeated f k)
    (if (= k 0)
        f
        (smooth (smooth-repeated f (dec k)))
    )
)

((smooth square) 2)

((smooth-repeated square 5) 2)
