#lang sicp

#|
(define (count-change amount)
    (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount 
                 (- kinds-of-coins 1))
             (cc (- amount
                    (first-denomination kinds-of-coins))
                  kinds-of-coins)))))    

(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50))
)

(count-change 100)

|#

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))


(define (cc amount coins-values)
    (cond ((= amount 0) 1)
          ((or (< amount 0) (no-more? coins-values)) 0)
          (else 
            (+ (cc amount (except-first-denomination coins-values))
               (cc (- amount (first-denomination coins-values)) coins-values)
            )
          )
    )
)

(define (first-denomination coins-values)
    (car coins-values)
)

(define (except-first-denomination coins-values)
    (cdr coins-values)
)

(define (no-more? coins-values)
    (null? coins-values)
)

(cc 100 us-coins)
(cc 100 (reverse us-coins))
(cc 100 uk-coins)