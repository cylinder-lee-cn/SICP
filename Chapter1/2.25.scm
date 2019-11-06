#lang sicp

(define a (list 1 3 (list 5 7) 9))
a

(define b (list (list 7)))

b

(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

c

; (car (cdr (car (cdr (cdr a)))))
(car (cdaddr a))
(caar b)
(cadadr (cadadr (cadadr c)))