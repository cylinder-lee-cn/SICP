#lang sicp

(define balance 100)

(define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
                balance
        )
        "Insufficient funds"
    )
)

(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)
(newline)
(define new-withdraw
    (let ((balance 100))
        (lambda (amount)
            (if (>= balance amount)
                (begin (set! balance (- balance amount))
                       balance
                )
                "Insufficient funds"
            )
        )
    )
)

(new-withdraw 25)
(new-withdraw 50)
(new-withdraw 60)
(new-withdraw 15)
(newline)

(define (make-withdraw balance)
    (lambda (amount)
        (if (>= balance amount)
            (begin (set! balance (- balance amount))
                balance
            )
            "Insufficient funds"
        )
    )
)
;make-withdraw,make-withdraw1是两种不同的写法，但是等效
(define make-withdraw1
    (lambda (balance)
            (lambda (amount)
                (if (>= balance amount)
                    (begin (set! balance (- balance amount))
                        balance
                    )
                    "Insufficient funds"
                )
            )
    )

)

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 50)
(W2 70)
(W2 40)
(W1 40)

(define ww1 (make-withdraw1 100))
(define ww2 (make-withdraw1 100))
(ww1 5)
(ww1 15)


(newline)
(define (make-account balance)
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
    (define (dispatch m)
        (cond ((eq? m `withdraw) withdraw)
              ((eq? m `deposit) deposit)
              (else (error "Unkown request -- MAKE-ACCOUNT" m))
        )
    )
    dispatch
)

(define acc (make-account 100))
((acc `withdraw) 50)
((acc `withdraw) 60)
((acc `deposit) 40)
((acc `withdraw) 60)