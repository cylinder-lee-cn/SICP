#lang sicp

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 1)
          (else (+ (count-leaves (car tree))
                (count-leaves (cdr tree))
          ))
    )
)

(count-leaves x)

(define (scale-tree tree factor)
      (cond ((null? tree) nil)
            ((not (pair? tree)) (* tree factor))
            (else
                  (cons (scale-tree (car tree) factor)
                        (scale-tree (cdr tree) factor)
                  )
            )
      )
)

(scale-tree (list 1 (list 2 (list 3 4) 5 (list 6 7))) 10)

(define (scale-tree1 tree factor)
      (map (lambda (sub-tree) 
            (if (pair? sub-tree)
                  (scale-tree1 sub-tree factor)
                  (* sub-tree factor)
            )
           )
           tree
      )
)

(scale-tree1 (list 1 (list 2 (list 3 4) 5 (list 6 7))) 10)