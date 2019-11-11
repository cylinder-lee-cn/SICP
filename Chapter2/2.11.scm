#lang sicp

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
                   (+ (upper-bound x) (upper-bound y))
    )
)

; (define (mul-interval x y)
;     (let ((p1 (* (lower-bound x) (lower-bound y)))
;           (p2 (* (lower-bound x) (upper-bound y)))
;           (p3 (* (upper-bound x) (lower-bound y)))
;           (p4 (* (upper-bound x) (upper-bound y)))
;          )
;          (make-interval (min p1 p2 p3 p4)
;                         (max p1 p2 p3 p4)
;          )
;     )
; )

#|
(define (div-interval x y)
    (mul-interval x
        (make-interval (/ 1.0 (upper-bound y))
                       (/ 1.0 (lower-bound y))
        )
    )
)
|#

(define (make-interval a b) (cons a b))
(define (lower-bound x) (min (car x) (cdr x)))
(define (upper-bound x) (max (cdr x) (cdr x)))

(define (sub-interval x y)
    (make-interval (- (lower-bound x) (upper-bound y))
                   (- (upper-bound x) (lower-bound y))
    )
)

(define (width-interval x)
    (/ (- (upper-bound x) (lower-bound x)) 2.0)
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
