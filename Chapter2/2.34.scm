#lang sicp

#|
多项式求值
1+3x+5x^3+x^5
系数列表(1 3 0 5 0 1),2次方和4次方都没有，所以系数是0
|#
(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (horner-eval x coefficient-sequence)
    (accumulate (lambda (this-coeff higher-terms) 
                    (+ (* higher-terms x) this-coeff)
                )
        0
        coefficient-sequence
    )
)

(define alist (list 1 3 0 5 0 1))

(horner-eval 2 alist)
