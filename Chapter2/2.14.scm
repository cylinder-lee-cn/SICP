#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (make-center-width c w)
    (make-interval (- c w) (+ c w))
)

(define (center i)
    (/ (+ (lower-bound i) (upper-bound i)) 2)
)
(define (width i)
    (/ (- (upper-bound i) (lower-bound i)) 2)
)

(define (make-center-percent c p)
    (make-interval (* c (- 1 p)) (* c (+ 1 p)))
)

(define (make-center-percent1 c p)
    (make-center-width c (* c p))
)
;make-center-percent1的效率应该更好一些，只做了一次乘法

(define (percent i)
    (/ (width i) (center i))
)

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))
    )
)

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))
    )
)

(define (span-zero? x)
    (and (<= (lower-bound x) 0) (>= (upper-bound x) 0))
)

(define (div-interval x y)
    (if (span-zero? y)
        (error "cross 0!")
        (mul-interval x
           (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))
           )
        )
    )
)

(define (positive-interval? x)
    (and (positive? (lower-bound x)) (positive? (upper-bound x)))
)
(define (negative-interval? x)
    (and (negative? (lower-bound x)) (negative? (upper-bound x)))
)

(define (mul-interval x y)
    (let ((xl (lower-bound x))
          (xu (upper-bound x))
          (yl (lower-bound y))
          (yu (upper-bound y))
         )
         (cond ((and (positive-interval? x) (positive-interval? y)) (make-interval (* xl yl) (* xu yu)))
               ((and (negative-interval? x) (negative-interval? y)) (make-interval (* xu yu) (* xl yl)))
               ((and (positive-interval? x) (negative-interval? y)) (make-interval (* xu yl) (* xl yu)))
               ((and (negative-interval? x) (positive-interval? y)) (make-interval (* xl yu) (* xu yl)))
               ((and (positive-interval? x) (span-zero? y)) (make-interval (* xu yl) (* xu yu)))
               ((and (negative-interval? x) (span-zero? y)) (make-interval (* xl yu) (* xl yl)))
               ((and (span-zero? x) (positive-interval? y)) (make-interval (* xl yu) (* xu yu)))
               ((and (span-zero? x) (negative-interval? y)) (make-interval (* xu yl) (* xl yl)))
               (else (make-interval (min (* xl yu) (* xu yl)) (max (* xl yl) (* xu yu))))
         )
    )
)

(define ra (make-interval 2 8))
(define rb (make-interval 3 7))

(define (pair1 r1 r2)
    (div-interval (mul-interval r1 r2) (add-interval r1 r2))
)

(define (pair2 r1 r2)
    (let ((one (make-interval 1 1)))
        (div-interval one 
                      (add-interval (div-interval one r1) (div-interval one r2))
        )
    )
)

(pair1 ra rb)
(pair2 ra rb)

(define p1 (make-center-percent1 5 0.03))
(define p2 (make-center-percent1 5 0.03))
(define p3 (make-center-percent1 6 0.02))

(pair1 p1 p2)
(pair2 p1 p2)

(div-interval p1 p2)
(div-interval p1 p3)

#|
Lem是正确的，由于计算每个因子就是一个区间，那么引用区间的次数越少结果就会越精确。

所以A*B/(A+B)的精确度不如 1/(1/A+1/B)
|#
