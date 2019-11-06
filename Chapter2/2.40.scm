#lang sicp

(#%require "../Chapter1/1.21.scm")

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (unique-pairs n)
    (flatmap (lambda (i) 
                (map (lambda (j) (list i j))
                    (enumerate-interval 1 (dec i))
                )
             )
            (enumerate-interval 1 n)
    )
)

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

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum? (unique-pairs n))
    )
)

(prime-sum-pairs 6)

(unique-pairs 6)