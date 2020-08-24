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

    (define (wrong-password x)
        ; 这个x参数没用，就是为了将wrong-password表示成为一个可以接收参数的过程
        (display "Incorrect password!")
        (newline)
    )
    (define (call-the-cops x)
        ; 这个x参数没用，就是为了将wrong-password表示成为一个可以接收参数的过程
        (display "Call-The-Cops!")
        (newline)
    )

    (let ((count 0))
        (define (dispatch pwd m)
            (if (eq? pwd password)
                (cond ((eq? m `withdraw) withdraw)
                    ((eq? m `deposit) deposit)
                    (else (error "Unkown request -- MAKE-ACCOUNT" m))
                )
                (if (< count 3)
                    (begin (set! count (inc count))
                    wrong-password
                    )
                    call-the-cops
                )
            )
        )
        dispatch
    )
)

(define acc (make-account 100 `secret-password))

((acc `secret-password `withdraw) 40)
((acc `secret-password `withdraw) 5)
; (newline)
((acc `some-other-password `deposit) 5)
((acc `password1 `deposit) 6)
((acc `password2 `deposit) 7)
((acc `password2 `deposit) 8)

#|
没有使用题设的7次输入密码判断，改成了3次
|#
