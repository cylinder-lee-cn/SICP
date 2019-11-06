#lang sicp

(define (make-rat n d)
    (let (
            (g (gcd n d))
        )
            (if (< (/ n d) 0)
                (cons (- (abs (/ n g))) (abs (/ d g)))
                (cons (abs (/ n g)) (abs (/ d g)))
            )
    )
)

(define (make-rat1 n d)
    (let ((g (gcd n d))
          (d-sign (if (< d 0) -1 1))
         )
         (cons (* (/ n g) d-sign) (* (/ d g) d-sign))
    )
)

(define (make-rat2 n d)
    (let ((g (gcd n d)))
        (if (< d 0)
            (cons (- (/ n g)) (- (/ d g)))
            (cons (/ n g) (/ d g))
        )
    )
)

(make-rat 3 -9)
(make-rat1 3 -9)
(make-rat2 -3 -9)
(make-rat2 3 9)
(make-rat2 3 -9)
#|
应该是make-rat1 更好，效率更高，make-rat2还可以进一步优化
|#
