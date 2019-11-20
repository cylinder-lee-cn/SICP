#lang sicp

(define right-split (split beside below))
(define up-split (split below beside))

(define (split comb2 comb1)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split comb2 comb1) painter (dec n))))
                (comb2 painter (comb1 smaller smaller))
            )
        )
    ) 
)

