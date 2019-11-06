#lang sicp

(define (average a b) (/ (+ a b) 2))
(define (square a) (* a a))
(define (cube a) (* a a a))

;平均阻尼的通用定义
(define (average-damp f)
  (lambda (x) (average x (f x)))
  )


((average-damp square) 10)

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

(fixed-point cos 1.0)

(display "1:")
(newline)
(define (sqrt1 x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0)
)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0)
  )

(sqrt1 10)
(newline)

(cube-root 10)


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

((deriv cube) 5)


(define (sqrt2 x)
  (nt-method (lambda (y) (- (square y) x)) 1.0)
  )

(sqrt2 10)

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess)
  )

(define (sqrt3 x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0
                            )
  )

(sqrt3 8)


(define (sqrt4 x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            nt-trans
                            1.0
                            )
  )

(sqrt4 8)

