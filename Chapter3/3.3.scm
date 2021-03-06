#lang sicp

(define (make-account balance password)
    (define (withdraw amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance
            )
            "Insufficient funds"
        )
    )
    (define (deposit amount)
        (set! balance (+ balance amount))
        balance
    )
    (define (dispatch pwd m)
        (if (eq? pwd password)
            (cond ((eq? m `withdraw) withdraw)
                  ((eq? m `deposit) deposit)
                  (else (error "Unkown request -- MAKE-ACCOUNT" m))
            )
            (error "Incorrect password!")
        )
        
    )
    dispatch
)

(define acc (make-account 100 `secret-password))

; ((acc `secret-password `withdraw) 40)
; (newline)
((acc `some-other-password `deposit) 50)