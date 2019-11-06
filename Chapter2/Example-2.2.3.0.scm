#lang sicp

(#%require "1.21.scm")

(define (square x) (* x x))

(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree)) 
            (if (odd? tree)
                (square tree)
                0
            )
          )
          (else 
            (+ (sum-odd-squares (car tree))
                (sum-odd-squares (cdr tree))
            )
          )
    )
)

(define x (cons (list 1 2) (list 3 4)))

(sum-odd-squares x)

(define (even-fibs n)
    (define (next k)
        (if (> k n)
            nil
            (let ((f (fib k)))
                (if (even? f)
                    (cons f (next (inc k)))
                    (next (inc k))
                )
            )
        )
    )
    (next 0)
)

(define (fib n)
  (cond ((= 0 n) 0)
        ((= 1 n) 1)
        (else (+ (fib (- n 1)) (fib (- n 2)))))
  )

(even-fibs 10)
(define tlist (list 1 2 3 4 5))

(map square tlist)

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence)) 
            (cons (car sequence) (filter predicate (cdr sequence)))
          )
          (else 
            (filter predicate (cdr sequence))
          )
    )    
)
(filter odd? tlist)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(accumulate + 0 tlist)
(accumulate * 1 tlist)
(accumulate cons nil tlist)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))
    )
)
(enumerate-interval 2 9)

(define (enumerate-tree tree)
    (cond ((null? tree) nil)
          ((not (pair? tree)) (list tree))
          (else
            (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree)))
          )
    )
)

(define atree (list 1 (list 2 (list 3 4)) 5))

atree

(enumerate-tree atree)

(define (sum-odd-square1 tree)
    (accumulate + 0 (map square (filter odd? (enumerate-tree tree))))
)

(sum-odd-square1 atree)

(define (list-fib-squares n)
    (accumulate cons nil (map square (map fib (enumerate-interval 0 n))))
)

(list-fib-squares 10)

(define (product-of-squares-of-odd-elements sequence)
    (accumulate * 1 (map square (filter odd? sequence)))
)

(product-of-squares-of-odd-elements tlist)
