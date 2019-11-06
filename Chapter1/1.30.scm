#lang sicp
#|
h3(y0+4y1+2y2+4y3+2y4+⋯+2yn−2+4yn−1+yn),

h=(b−a)/n

yk=f(a+kh) 
|#
(define (cube x) (* x x x))

; (define (sum term a next b)
;     (if (> a b)
;         0
;         (+ (term a) (sum term (next a) next b))

;     )
; )

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
            result
            (iter (next a) (+ (term a) result))
        )
    )
    (iter a 0)
)

(define (sip f a b n)
    (define h (/ (- b a) n))
    (define (y k)
        (f (+ a (* k h)))
    )
    (define (term k)
        (* (y k)
            (cond ((or (= k 0) (= k n)) 1)
                  ((even? k) 2)
                  ((odd? k) 4)
            )
        )
    )
    (* (/ h 3) (sum term a inc n))
)

(sip cube 0 1 1000)