#lang sicp
(define (square x) (* x x))

(define (average a b) (/ (+ a b) 2))

(define (average-damp f)
    (lambda (x) (average x (f x)))
)

(define (** x k)
    (if (= k 1)
        x
        (* x (** x (dec k)))
    )
)

(define (compose f g)
    (lambda (x) (f (g x)))
)

(define (repeated g k)
    (if (= k 1)
        g
        (compose g (repeated g (dec k)))
    )
)

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

;x 是幂，e是开方次数，t是做平均阻尼的次数
(define (m-root x e t)
    ;(fixed-point (average-damp (lambda (y) (/ x (** y (- e 1))))) 1.0)
    (fixed-point ((repeated average-damp t) 
                    (lambda (y) (/ x (** y (- e 1))))) 1.0)
)

(m-root 243 5 2)

#|
e   t
-----
2   1
3	  1
4	  2
5	  2
6	  2
7	  2
8	  3
9	  3
10	3
11	3
12	3
13	3
14	3
15	3
16	4
17	4
…	  4
30	4
31	4
32	5
…	  5
63	5
64	6

t is (floor (log e 2))
以上结论如何推导而来，不知。
|#





