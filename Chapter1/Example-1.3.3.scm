#lang sicp
#|
寻找函数的零点，就是f(x)=0，也就是f(x)方程的根
|#
(define (close-enough? x y)
    (< (abs (- x y)) 0.001)
)

(define (average x y)
    (/ (+ x y) 2.0)
)

(define (search f neg-point pos-point)
    (let ((midpoint (average neg-point pos-point)))
        (if (close-enough? neg-point pos-point)
            midpoint
            (let ((test-value (f midpoint)))
                (cond ((positive? test-value) 
                        (search f neg-point midpoint)
                      )
                      ((negative? test-value)
                        (search f midpoint pos-point)
                      )
                      (else midpoint)
                )
            )
        )
    )
)

(define (half-interval-method f a b)
    (let ((a-value (f a))
          (b-value (f b))
         )
         (cond ((and (negative? a-value) (positive? b-value))
                (search f a b)
                )
               ((and (negative? b-value) (positive? a-value))
                (search f b a)
               ) 
               (else (error "Values are not of opposite sign" a b))
         )
    )
)

; (search sin 2.0 4.0)
(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 1.0 2.0)

#|
这一段是找出函数的不动点，x取值可以满足f(x)=x。简单的是f是x^2，x=1时满足f(x)=x=1
|#

(define (fixed-point f first-guess)
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                next
                (try next)
            )
        )
    )
    (try first-guess)
)

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)
    (fixed-point (lambda (y) (average y (/ x y))) 1.0)
)

(sqrt 10)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0)