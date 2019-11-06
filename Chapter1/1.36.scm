#lang sicp
(define (fixed-point f first-guess)
    (define (close-enough? a b)
        (< (abs (- a b)) 0.00001)
    )
    (define (try guess)
        (let ((next (f guess)))
            (display next)
            (newline)
            (if (close-enough? guess next)
                next
                (try next)
            )
        )
    )
    (try first-guess)
)

; (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)

#|
average 其实就是书中的平均阻尼 (x+f(x))/2 作为下一个使用x值，不是直接是用f(x)
推导过程，目标是f(x)=x,两边同时加x，
f(x)+x=x+x
f(x)+x=2x
(f(x)+x)/2=x
|#

(define (average a b)
    (/ (+ a b) 2.0)
)

(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 10.0)