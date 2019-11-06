#lang sicp
(define alist (list 1 3 5 7 9))
(define blist (list 0 2 4 6 8))

(define (accumlate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumlate op initial (cdr sequence)))
    )
)

(define (map1 p sequence)
    (accumlate (lambda (x y) (cons (p x) y)) nil sequence)
)

(map1 sqrt alist)


(define (append1 seq1 seq2)
    (accumlate cons seq2 seq1)
)

(append1 alist blist)
(append alist blist)


(define (length1 sequence)
    (accumlate (lambda (x y) 
                    (if (null? x)
                        0
                        (inc y)
                    ) 
                ) 
                0 sequence)
)
(define (length2 sequence)
    (accumlate (lambda (x y) (inc y)) 0 sequence)
)

(length1 alist)
(length2 blist)

#|
(accumlate op init seq)
整个过程展开后就成了
(op seq-n-4 (op seq-n-3 (op seq-n-2 (op seq-n-1 (op seq-n init)))))...
如果定义一个lambda函数，lambda函数会有2个参数，lambda (x y), 初始的时候 x-> seq-n y-> init
根据这个可以设计lambda的函数体
#|