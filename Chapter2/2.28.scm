#lang sicp

(define x (list (list 1 2) (list 3 4)))

(define (fringe tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else 
            (append (fringe (car tree)) (fringe (cadr tree)))
          )
    )
)


x 
(list x x)

(fringe x)

(fringe (list x x))

