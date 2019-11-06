#lang sicp

#|
e-2 欧拉的连分式展开,Di依次是[1,2,1,1,4,1,1,6,1,1,8,...]
Di和i的对应关系是 
(i+1) mod 3 ==0 时 Di=(i+1)/3 * 2
|#

(define (cont-frac n d k)
    (define (cf i)
        (if (= i k)
            (/ (n k) (d k))
            (/ (n i) (+ (d i) (cf (inc i))))
        )
    )
    (cf 1)
)
#|
下面计算出来的是e-2
|#

(cont-frac  (lambda (i) 1.0) 
            (lambda (x) (if (= (remainder (+ x 1) 3) 0)
                (* 2 (/ (+ x 1) 3))
                1)
            ) 
            12)