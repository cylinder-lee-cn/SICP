#lang sicp
; 递归的解法
(define (product term a next b)
    (if (> a b)
        1.0
        (* (term a) 
            (product term (next a) next b)
        )
    )
)

;迭代的解法
(define (product1 term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (* (term a) result))
        )
    )
    (iter a 1)
)


(define (pi2 n)
    (define (term n)
        (* (/ (* 2.0 n) (- (* 2.0 n) 1.0))
           (/ (* 2.0 n) (+ (* 2.0 n) 1.0))
        )
    )
    (product1 term 1 inc n)
)

(* (pi2 1000) 2)