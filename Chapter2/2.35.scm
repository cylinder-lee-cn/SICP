#lang sicp

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else
            (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
          )
    )
)

(define (count-leaves t)
    (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t)))
)

(define atree (list 1 (list 2 (list 3 4))))
(define btree (list 1 (list 2 (list 3 4)) 5))

(define (count-leaves1 t)
    (accumulate + 
                0 
                (map (lambda (sub-tree)
                        (if (pair? sub-tree)
                            (count-leaves1 sub-tree)
                            1
                        )
                     )
                    t
                )
    )
)


(count-leaves atree)
(count-leaves btree)

(count-leaves1 atree)
(count-leaves1 btree)
