#lang sicp

(define (new-equal? a b)
    (cond ((and (null? a) (null? b)) #t)
          ((not (eq? (car a) (car b))) #f)
          (else
            (new-equal? (cdr a) (cdr b))
          )
    )
)

(new-equal? '(this is a list) '(this is a list))

(new-equal? '(this is a list) '(this (is a) list))