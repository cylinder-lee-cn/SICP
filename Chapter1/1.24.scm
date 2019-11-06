#lang sicp
(#%require "1.21.scm")

(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (square (expmod base (/ exp 2) m)) m)
          )
          (else 
            (remainder (* base (expmod base (- exp 1) m)) m)
          )
    )
)

(define (fermat-test n)
    (define (try-it a)
        (= (expmod a n n) a)
    )
    (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
    (cond ((= times 0) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)
    )
)
(define (square x)
    (* x x))

(define (prime? n)
    (fast-prime? n 10)
)


(define (next-odd n)
    (if (odd? n)
        (+ n 2)
        (+ n 1)
    )
)

(define (search-for-primes n count start-time)
    (cond ((= count 0) 
            (display (- (runtime) start-time))
          )
          ((prime? n)
              (display n)
              (newline)
              (search-for-primes (next-odd n) (- count 1) start-time)
          )
          (else (search-for-primes (next-odd n) count start-time))
    )
)

; (search-for-primes 100000000 12 (runtime))

(fast-prime? 561 10)