#lang sicp

(define (list-ref1 items n)
    (if (= n 0)
        items
        (list-ref1 (cdr items) (dec n))
    )
)

;迭代方式
(define (length2 items)
    (define (len items l)
        (if (null? items)
            l
            (len (cdr items) (inc l))
        )
    )
    (len items 0)
)

(define (last-pair1 items)
    (list-ref1 items (dec (length2 items)))
)

(define squares (list 1 4 9 16 25))
(define oneitem (list 2))

(last-pair1 squares)

#|
遍历一次
|#

(define (last-pair2 items)
    (if (null? (cdr items))
        items
        (last-pair2 (cdr items))
    )
)

(last-pair2 squares)
(last-pair2 oneitem)

