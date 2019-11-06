#lang sicp

(define (avg a b)
    (/ (+ a b) 2.0)
)

(define (square x) (* x x))

(define (make-point x y)
    (cons x y)
)
(define (x-point p)
    (car p)
)
(define (y-point p)
    (cdr p)
)

(define (make-segment s e)
    (cons s e)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (length-segment segment)
    (let ((a (- (x-point (end-segment segment)) (x-point (start-segment segment))))
          (b (- (y-point (end-segment segment)) (y-point (start-segment segment))))
         )
        (sqrt (+ (square a) (square b)))
    )
)

(length-segment (make-segment (make-point 1 2) (make-point 4 6)))

#|
lt-point 是左上角座标，起始是0,0
rb-point 是右下角座标，最大是屏幕分辨率，比如 1024,768
lb-point 是左下角座标
rt-point 是右上角座标
|#

(define (make-rectangle lt-point rb-point)
    (cons lt-point rb-point)
)

(define (get-lt-point rectangle)
    (car rectangle)
)
(define (get-rb-point rectangle)
    (cdr rectangle)
)
(define (get-lb-point rectangle)
    (make-point
        (x-point (get-lt-point rectangle))
        (y-point (get-rb-point rectangle))
    )
)
(define (get-rt-point rectangle)
    (make-point
        (x-point (get-rb-point rectangle))
        (y-point (get-lt-point rectangle))
    )
)

(define (get-l-segment rectangle)
    (make-segment
        (get-lt-point rectangle)
        (get-lb-point rectangle)
    )
)
(define (get-w-segment rectangle)
    (make-segment
        (get-lt-point rectangle)
        (get-rt-point rectangle)
    )
)

(define (get-perimeter rectangle)
    (* (+ (length-segment (get-l-segment rectangle))
          (length-segment (get-w-segment rectangle))) 2)
)

(get-perimeter (make-rectangle (make-point 0 0) (make-point 4 5)))

(define (get-area rectangle)
    (* 
        (length-segment (get-l-segment rectangle))
        (length-segment (get-w-segment rectangle))
    )
)

(get-area (make-rectangle (make-point 0 0) (make-point 4 5)))