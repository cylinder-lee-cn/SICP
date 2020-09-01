#lang sicp

(define (everynumber n sum-ret mul-ret)
    (if (= 0 n)
        (- mul-ret sum-ret)
        (let ((a (remainder n 10))
              (m (quotient n 10))
             )
             (everynumber m (+ sum-ret a) (* mul-ret a))
        )
    )
)

