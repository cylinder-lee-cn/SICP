#lang sicp

(define (for-each proc items)
    (cond ((null? items) (newline) (display "Done!"))
          (else
            (proc (car items))
            (for-each proc (cdr items))
          )
    )
)

(for-each (lambda (x) (newline) (display x)) (list 23 44 77 88))