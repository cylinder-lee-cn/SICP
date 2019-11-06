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

(define alist (list 1 2 3 4 5))

(define (reverse1 sequence)
    (fold-right (lambda (x y) 
                    (append y (list x))
                )
                nil
                sequence
    )
)

(define (reverse2 sequence)
    (fold-left (lambda (x y)
                    (cons y x)
                )
                nil
                sequence
    )
)

(reverse1 alist)
(reverse2 alist)

#|
使用append 和 cons就能形成两种不同的顺序，分别用于right和left，可以输出相同的结果
|#
