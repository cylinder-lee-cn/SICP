#lang sicp

#|
按照索引去访问List
|#

(define (list-ref items n)
    (if (= n 0)
        (car items)
        (list-ref (cdr items) (dec n))
    )
)

(define squares (list 1 4 9 16 25))
(define odds (list 1 3 5 7 9))

(list-ref squares 3)

;递归方式
(define (length1 items)
    (if (null? items)
        0
        (+ 1 (length1 (cdr items)))
    )
)
;迭代方式
(define (length2 items)
    (define (len items l)
        (if (null? items)
            l
            (len (cdr items) (inc l))
        )
    )
    (len items 0)
)

(length2 squares)

(define (append1 list1 list2)
    (if (null? list1)
        list2
        (cons (car list1) (append (cdr list1) list2))
    )
)

(append squares odds)
(append odds squares)


#|
对表的映射
|#

(define (scale-list items factor)
    (if (null? items) 
        nil
        (cons (* (car items) factor) (scale-list (cdr items) factor))
    )
)

(scale-list (list 1 2 3 4 5) 10)

(define (map proc items)
    (if (null? items)
        nil
        (cons (proc (car items))
              (map proc (cdr items))
        )
    )
)

(map abs (list -10 2.5 -11.5 17))

(map (lambda (x) (* x x)) (list 1 2 3 4))

(define (scale-list1 items factor)
    (map (lambda (x) (* x factor)) items)
)

(scale-list (list 1 3 5 7 9) 10)