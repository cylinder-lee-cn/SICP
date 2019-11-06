#lang sicp

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items))
        )
    )
)

(define (square-list1 items)
    (if (null? items)
        nil
        (cons (* (car items) (car items)) (square-list1 (cdr items)))
    )
)

(define (square-list2 items)
    (map (lambda (x) (* x x)) items)
)

(square-list1 (list 1 2 3 4))
(square-list2 (list 1 2 3 4))
