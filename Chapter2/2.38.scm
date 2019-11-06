#lang sicp

(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) (cdr rest))
            ; (iter (op (car rest) result) (cdr rest))
        )
    )
    (iter initial sequence)
)

(define (fold-right op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (fold-right op initial (cdr sequence)))
    )
)

(fold-right / 1 (list 1 2 3)) ;3/2
(fold-left / 1 (list 1 2 3)) ;1/6
(fold-right list nil (list 1 2 3)) ;(1 (2 (3 ())))
(fold-left list nil (list 1 2 3)) ;(((() 1) 2) 3)

#|
op操作符，必须满足交换律，也就是(op A B) == (op B A)
这样可以保证fold-left和fold-right能获得同样的结果，否则不行
|#
