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

(define (deep-reverse items)
    (define (dr elements temp)
        (if (null? elements)
            temp
            (dr (cdr elements) (cons (reverse1 (car elements)) temp))
        )
    )
    (dr items nil)
)


(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1) (list 2 3 4)))

x
y

(reverse1 x)
(reverse1 y)

(deep-reverse x)
(deep-reverse y)