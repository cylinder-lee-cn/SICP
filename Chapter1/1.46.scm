#lang sicp

;改写sqrt
(define (average a b)
    (/ (+ a b) 2.0))

(define (square x)
    (* x x))

(define (iterative-improve good? improve)
    (define (try guess)
        (let ((next (improve guess)))
        (if (good? next)
            next
            (try next))))
    (lambda (guess) (try guess))
  )

(define (sqrta x)
    (define (good-enough? guess)
        (< (abs (- (square guess) x)) 0.00001))
    (define (improve-guess guess)
        (average guess (/ x guess)))
    ((iterative-improve good-enough? improve-guess) 1.0)
)

(sqrta 9)
(newline)
;改写fixed-point

(define (fixed-point f first-guess)

    (define (close-enough? x)
        (< (abs (- x (f x))) 0.00001))

    (define (improve x)
        (f x))

    ((iterative-improve close-enough? improve) first-guess)
)

(fixed-point cos 1.0)
(newline)