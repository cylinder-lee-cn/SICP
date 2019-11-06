#lang sicp

; (define (same-parity . n)
;     (let ((s (remainder (length n) 2)))
;         (display s)
;         (newline)
;         (get-items 0 n nil s)
;     )
; )

; (define (get-items idx elements temp sign)
;     (cond ((null? elements) temp)
;           ((= sign (remainder idx 2)) (get-items (inc idx) (cdr elements) (cons (car elements) temp) sign))
;           (else (get-items (inc idx) (cdr elements) temp sign))
;     )
; )

(define (same-parity . n)
    (let ((ns (odd? (car n))))
        (get-items n nil ns)
    )
)
;迭代方式会导致逆序
(define (get-items elements temp sign)
    (cond ((null? elements) temp)
          ((equal? (odd? (car elements)) sign) 
            (get-items  (cdr elements) (cons (car elements) temp) sign))
          (else (get-items (cdr elements) temp sign))
    )
)

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

(define (get-items1 elements sign)
    (cond ((null? elements) nil)
          ((equal? (odd? (car elements)) sign) 
            (cons (car elements) (get-items1 (cdr elements) sign)))
          (else (get-items1 (cdr elements) sign))
    )
)

(get-items1 (list 1 2 3 4 5 6 7) #t)
(get-items1 (list 2 3 4 5 6 7 8) #f)
