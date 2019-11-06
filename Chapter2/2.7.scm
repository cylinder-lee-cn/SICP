#lang sicp

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))
    )
)

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
          (p2 (* (lower-bound x) (upper-bound y)))
          (p3 (* (upper-bound x) (lower-bound y)))
          (p4 (* (upper-bound x) (upper-bound y)))
         )
         (make-interval (min p1 p2 p3 p4)
                        (max p1 p2 p3 p4)
         )
    )
)

(define (div-interval x y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))
        )
    )
)

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))
    )
)

(define (width-interval a)
    (/ (- (upper-bound a) (lower-bound a)) 2.0)
)


(define p (make-interval 20 22))
(define q (make-interval 37 43))


(sub-interval p q) (add-interval p q)

