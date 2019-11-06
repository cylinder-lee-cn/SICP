#lang sicp

#|
把一个数对表示成为2^a*3^b，然后还要从里面把a和b拆出来。

那就是将结果连续除以2，看看有几次能被2整除
然后再连续除以3，看看有几次能被3整除
|#


(define (cons a b)
    (*  (expt 2 a)
        (expt 3 b)
    )
)

(define (car x)
    (if (= 0 (remainder x 2))
        (+ 1 (car (/ x 2)))
        0
    )
)

(define (cdr x)
    (if (= 0 (remainder x 3))
        (+ 1 (car (/ x 3)))
        0
    )
)

(define (car1 x)
    (define (car-i i x)
        (if (= 0 (remainder x 2))
            (car-i (inc i) (/ x 2))
            i
        )
    )
    (car-i 0 x)
)
(define (cdr1 x)
    (define (cdr-i i x)
        (if (= 0 (remainder x 3))
            (cdr-i (inc i) (/ x 3))
            i
        )
    )
    (cdr-i 0 x)
)

(cons 3 2)
(car 72)
(car1 72)
(cdr1 72)