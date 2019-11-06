#lang sicp

(define zero (lambda (f) (lambda (x) x))
)

(define (add-1 n)
    (lambda (f) (lambda (x) (f ((n f) x))))
)

(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (ch-add a b)
    (lambda (f) 
        (lambda (x) ((a f) ((b f) x)))
    )
)

(((ch-add one two) inc) 2)

(define (ch-mul a b)
    (lambda (f)
        (lambda (x) ((a (b f)) x))
    )
)

(define (ch-exp a b)
    (lambda (f)
        (lambda (x) (((a b) f) x))
    )
)

(((ch-mul two three) inc) 2)
(((ch-exp two three) inc) 2)

