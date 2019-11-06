#lang sicp

(define (square-tree tree)
    (tree-map square tree)
)
(define (square-tree1 tree)
    (tree-map1 square tree)
)

(define (square x) (* x x))

(define (tree-map proc tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (proc tree))
          (else
            (cons (tree-map proc (car tree)) (tree-map proc (cdr tree)))
          )
          
    )
)
;这种使用map以及lambda的方式更为清晰和优雅
(define (tree-map1 proc tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (tree-map1 proc sub-tree)
                (proc sub-tree)

            )
        )
        tree 
    )
)

(define atree (list 1 (list 2 (list 3 4) 5) (list 6 7)))

(square-tree atree)
(square-tree1 atree)