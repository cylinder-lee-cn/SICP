#lang sicp

(define atree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

;第一种定义方法，参考书的第一个例子，用cond进行条件判断

(define (square-tree1 tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (* tree tree))
          (else
            (cons (square-tree1 (car tree)) (square-tree1 (cdr tree)))
          )
    )
)

(square-tree1 atree)

;第二种定义方法，参考书的第二个例子，使用lambda和map

(define (square-tree2 tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree2 sub-tree)
                (* sub-tree sub-tree)
            )
         )
         tree
    )
)

(square-tree2 atree)