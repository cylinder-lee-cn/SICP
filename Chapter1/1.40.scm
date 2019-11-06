#lang sicp
(define (cube x) (* x x x))
(define (square x) (* x x))
(define (average a b) (/ (+ a b) 2))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 0.00001)
    )
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


(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx))
)

(define (nt-trans g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))
    )
  )

(define (nt-method g guess)
  (fixed-point (nt-trans g) guess)
  )

(define (cubic a b c)
    (lambda (x) 
        (+ (cube x) (* a (square x)) (* b x) c)
    )
)

(nt-method (cubic 3 2 1) 1.0)  

(nt-method (cubic 1 2 3) 1.0)  