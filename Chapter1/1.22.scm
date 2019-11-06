#lang sicp
(#%require "1.21.scm")

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


(search-for-primes 1000 3 (runtime))

; (search-for-primes 10000 3 (runtime))
; (search-for-primes 100000 3 (runtime))
; (search-for-primes 1000000 3 (runtime))

; (define (timed-prime-test n)
;     (newline)
;     (display n)
;     (start-prime-test n (runtime))
; )

; (define (start-prime-test n start-time)
;     (if (prime? n) 
;         (report-prime (- (runtime) start-time)))
; )

; (define (report-prime elapsed-time)
;     (display " *** ")
;     (display elapsed-time)
;     (newline)
; )


; (define (search-for-primes n start-time count)
;     (cond ((= count 0) (display elapsed-time) )
;           ((prime? n) (show (+ n 1) (- count 1)))
;           (else (search-for-primes (+ n 1) count))
;     )
; )

; (define (show n count) 
;     (newline)
;     (display (- n 1))
;     (search-for-primes n count)
; )

