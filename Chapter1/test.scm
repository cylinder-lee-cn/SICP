#lang sicp
(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))

#|

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b
       )
  )
|#
;(define (plus4 x) (+ x 4))

((lambda (x) (* x x)) 4)

(define plus4 (lambda (x) (+ x 4)))

(plus4 10)

(define (square x) (* x x))

((lambda (x y z) (+ x y (square z))) 1 2 3)

(define (f0 x y)
  (define (f-helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b))
    )
  (f-helper (+ 1 (* x y))
            (- 1 y))
  )

(define (f1 x y)
  ((lambda (a b)
     (+ (* x (square a))
        (* y b)
        (* a b)))
   (+ 1 (* x y))
   (- 1 y)
   )
  )

(define (f2 x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y))
        )
    (+ (* x (square a))
       (* y b)
       (* a b))
    )
  )
(define x 5)

(+ (let ((x 3))
     (+ x (* x 10))
     )
   )


(let ((x 3)
      (y (+ x 2))
      )
  (* x y)
  )

((lambda (x y) (* x y))
 3
 (+ x 2))


(define (f g)
  (g 2))
(f square)
(f (lambda (z) (* z (+ z 1))))


;(error "test error")

(display "---------------")
(newline)

(define (double f)
  (lambda (x)
    (f (f x))
    )
  )

(display "---------------")
(newline)

(define (compose f g)
  (lambda (x) (f (g x)))
  )

(define (repeated g k)
  (if (= k 1)
      g
      (compose g (repeated g (dec k)))
      )
  )

((repeated square 2) 5)