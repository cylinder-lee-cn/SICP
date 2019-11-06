#lang sicp

(define (f n d k)
    (if (= 0 k)
        (/ n d)
        (f n (+ 1.0 (/ 1.0 d)) (dec k))
    )
)

(define (f1 n d k)
    (define (iter d result k)
        (if (= 0 k)
            result
            (iter (+ 1 result) (/ 1 (+ 1 result)) (dec k))
        )
    )
    (iter d 1.0 k)
)

(f 1.0 1.0 10)
(f1 1.0 1.0 100)
#|
cf(1)
N1/(D1+cf(2))
N1/(D1+(N2/(D2+cf(3))))
N1/(D1+(N2/(D2+(N3/(D3+cf(4))))))
⋯
N1/(D1+(N2/(D2+(N3/(D3+⋯+(Nk/Dk))))))
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

(cont-frac  (lambda (i) 1.0) 
            (lambda (i) 1.0) 
            11)

#|

|#
(newline)

(define (cont-frac1 n d k)
    (define (iter k result)
        (newline)
        (display result)
        (if (= k 0)
            result
            (iter (dec k) (/ (n k) (+ (d k) result)))
        )
    )
    (iter k 0)
)

(cont-frac1  (lambda (i) 1.0) 
            (lambda (i) 1.0) 
            11)