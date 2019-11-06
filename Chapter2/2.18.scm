#lang sicp

(define (reverse1 items)
    (define (r elements temp)
        (if (null? elements)
            temp
            (r (cdr elements) (cons (car elements) temp))
        )
    )
    (r items nil)
)

(define (reverse2 items)
    (if (null? items)
        nil
        (cons (car items) (reverse2 (cdr items)))
        ; (append (reverse2 (cdr items)) (list (car items)))
    )
)

(define odds (list 1 3 5 7 9))

(reverse1 odds)

(reverse2 odds)

#|
用递归也可以写出逆序，不用cons用append即可
|#
