#lang sicp

(#%require sicp-pict)

;(paint einstein)
;(paint diagonal-shading)

(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside smaller smaller))
        )
    )
)

(define (corner-split painter n)
    (if (= n 0)
        painter
        (let ((up (up-split painter (- n 1)))
              (right (right-split painter (- n 1))))
        
          (let (
                (top-left (beside up up))
                (bottom-right (below right right))
                (corner (corner-split painter (- n 1)))
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
        (let ((half (beside (flip-horiz quarter) quarter)))
            (below (flip-vert half) half)
        )
    )
)

;(paint (right-split einstein 3))
;(paint (up-split einstein 3))
;
;(paint (corner-split einstein 3))
;
;(paint (square-limit einstein 3))
;
;(paint (flip-horiz einstein))

(define (square-of-four tl tr bl br)
    (lambda (painter)
        (let (
                (top (beside (tl painter) (tr painter)))
                (bottom (beside (bl painter) (br painter)))
             )
             (below bottom top)
        )
    )
)

(define (square-limit1 painter n)
    (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
        (combine4 (corner-split painter n))
    )
)

(paint ((square-of-four flip-horiz identity rotate180 flip-vert) einstein))
(paint ((square-of-four identity flip-horiz flip-vert rotate180) einstein))
;(paint (square-limit1 einstein 2))




;(define (split comb2 comb1)
;    (lambda (painter)
;        (comb2 painter (comb1 painter painter))
;    ) 
;)
(define (split comb2 comb1)
    (lambda (painter n)
        (if (= n 0)
            painter
            (let ((smaller ((split comb2 comb1) painter (- n 1))))
                (comb2 painter (comb1 smaller smaller))
            )
        )
    ) 
)



;(define right-split1 (split beside below))
;(define up-split1 (split below beside))
;
;(paint (right-split1 einstein 3))
;(paint (up-split1 einstein 3))

(paint (rotate90 (beside (rotate270 einstein) (rotate270 einstein))))

