#lang sicp
#|
系数是
1 3 5 7 9 11 ……
对应k
1 2 3 4 5 6 ……
Di=K*2-1
而且当k==1 -> x
k>1时 -> x^2
|#

(define (cont-frac n d k)
    (define (cf i)
        (if (= i k)
            (/ (n k) (d k))
            (/ (n i) (- (d i) (cf (inc i))))
        )
    )
    (cf 1)
)

(define (tan-cf x k)
    (define (n i)
        (if (= i 1)
            x
            (* x x)
        )
    )
    (define (d i)
        (- (* i 2) 1)
    )

    (cont-frac n d k)
)

(tan-cf 2.0 10)
(tan-cf 3.0 10)