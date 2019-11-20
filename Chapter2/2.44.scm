#lang sicp

(define (right-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (right-split painter (dec n))))
            (beside painter (below smaller smaller))
        )
    )
)

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (dec n))))
            (below painter (beside smaller smaller))
        )
    )
)

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (dec n)))
              (right (right-split painter (dec n))))
            (let (
                    (top-left (beside up up))
                    (bottom-right (below right right))
                    (corner (corner-split painter (dec n)))
                )
                (beside (below painter top-left)
                        (below bottom-right corner)
                )
            )
        )
    )
)

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
        (let ((half (beside (flip-horize quarter) quarter)))
            (below (flip-vert half) half)
        )
    )
)