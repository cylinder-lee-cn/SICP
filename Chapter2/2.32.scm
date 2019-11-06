#lang sicp

(define (subsets s)
    (if (null? s)
        (list nil)
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x)) rest))
        )
    )
)

(define alist (list 1 2 3))

(subsets alist)

#|
(subsets '(1 2 3))
rest ← (subsets '(2 3))
       rest ← (subsets '(3))
              rest ← (subsets '())
                     '(())
              (append '(()) (map ⟨…⟩ '(())))
              '(() (3))
       (append '(() (3)) (map ⟨…⟩ '(() (3))))
       '(() (3) (2) (2 3))
(append '(() (3) (2) (2 3)) (map ⟨…⟩ '(() (3) (2) (2 3))))
'(() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))


'(())               ⟼ '((3))                     given s = '(3)
'(() (3))           ⟼ '((2) (2 3))               given s = '(2 3)
'(() (3) (2) (2 3)) ⟼ '((1) (1 3) (1 2) (1 2 3)) given s = '(1 2 3)

(list 1)
rest=()
(car (list 1)) =1
map->(cons 1 (list nil))=((1))
append (list nil) `((1))->(() (1))
|#
