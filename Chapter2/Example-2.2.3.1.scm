#lang sicp

(#%require "../Chapter1/1.21.scm")

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))
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

; (accumulate append 
;             nil 
;             (map (lambda (i)
;                     (map (lambda (j) (list i j))
;                         (enumerate-interval 1 (dec i))
;                     )
;                  )
;                  (enumerate-interval 1 n)
;             )
; )

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

; (define (prime-sum-pairs n)
;     (map make-pair-sum 
;         (filter prime-sum?
;             (flatmap (lambda (i) 
;                 (map (lambda (j) (list i j)) (enumerate-interval 1 (dec i)))
;             )
;             (enumerate-interval 1 n)
;             ))
;     )
; )
(define (prime-sum-pairs n)
    (map make-pair-sum
        (filter prime-sum?
            (flatmap (lambda (i) 
                        (map (lambda (j) (list i j))
                            (enumerate-interval 1 (dec i))
                        )
                    )
                    (enumerate-interval 1 n)
            )
        )
    )
)

(prime-sum-pairs 6)

(define (remove item sequence)
    (filter (lambda (x) (not (= x item))) sequence)
)

(remove 3 (list 1 2 3))

(define (permutations s)
    (if (null? s)
        (list nil)
        (flatmap (lambda (x) 
                    (map (lambda (p) (cons x p)) 
                        (permutations (remove x s))
                    )
        ) s)
    )
)

(permutations (list 1 2 3))



