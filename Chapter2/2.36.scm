#lang sicp

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs))
        )
    )
)

(define sss (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

(accumulate-n + 0 sss)

#|
((1 2 3) (4 5 6) (7 8 9) (10 11 12))
需要转换成 ((1 4 7 10) (2 5 8 11) (3 6 9 12))就可以计算了。
利用map
(map car seqs)-> (1 4 7 10)
(map cdr seqs)-> ((2 3) (5 6) (8 9) (11 12))
|#
