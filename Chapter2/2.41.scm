#lang sicp

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

(define (unique-pairs3 n)
    (flatmap (lambda (i) 
                (flatmap (lambda (j)
                            (map (lambda (k) (list i j k))
                                (enumerate-interval 1 (dec j))
                            )
                         )
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

(define (sum-equal? seq s)
    (= s (+ (car seq) (cadr seq) (caddr seq)))
)

(unique-pairs3 6)

; (sum-equal? (list 5 4 2))

(define (filter-sum seqs sss)
    (filter (lambda (sq) (sum-equal? sq sss)) seqs)
)

(filter-sum (unique-pairs3 6) 10)

