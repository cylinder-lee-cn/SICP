#lang sicp

(#%require sicp-pict)

; (paint einstein)
; (paint diagonal-shading)

(define wave2 (beside wave (flip-vert wave)))
(define wave4 (below wave2 wave2))

;将wave4使用过程抽象
(define (flipped-pairs painter)
    (let ((painter2 (beside painter (flip-vert painter))))
        (below painter2 painter2)
    )
)

;重新定义wave4
(define wave4 (flipped-pairs wave))

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
        )
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

(define (flipped-pairs1 painter)
    (let ((combine4 (square-of-four identity flip-vert identity flip-vert)))
        (combine4 painter)
    )
)
;等价下面的：
(define flipped-pairs2 
    (square-of-four identity flip-vert identity flip-vert)
)

(define (square-limit painter n)
    (let ((combine4 (square-of-four flip-horiz identity rotate180 flip-vert)))
        (combine4 (corner-split painter n))
    )
)

(define (frame-coord-map frame)
    (lambda (v)
        (add-vect 
            (origin-frame frame)
            (add-vert (scale-vect (xcor-vect v) (edge1-frame frame))
                      (scale-vect (ycor-vect v) (edge2-frame frame))
            )
        )
    )
)

(define (segments->painter segment-list)
    (lambda (frame) 
        (for-each (lambda (segment) 
                    (draw-line
                        ((frame-coord-map frame) (start-segment segment))
                        ((frame-coord-map frame) (end-segment segment))
                    )
                  )
                  segment-list
        )
    )
)

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
                (painter 
                    (make-frame new-origin
                                (sub-vect (m corner1) new-origin)
                                (sub-vect (m corner2) new-origin)
                    )
                )
            )
        )
    )
)


(define (flip-vect painter)
    (transform-painter painter
                       (make-vect 0.0 1.0)
                       (make-vect 1.0 1.0)
                       (make-vect 0.0 0.0)
    )
)

(define (shrink-to-upper-right painter)
    (transform-painter painter
                        (make-vect 0.5 0.5)
                        (make-vect 1.5 0.5)
                        (make-vect 0.5 1.0)
    )
)

(define (rotate90 painter)
    (transform-painter painter
                        (make-vect 1.0 0.0)
                        (make-vect 1.0 1.0)
                        (make-vect 0.0 0.0)
    )
)

(define (squash-inwards painter)
    (transform-painter painter
                        (make-vect 0.0 0.0)
                        (make-vect 0.65 0.35)
                        (make-vect 0.35 0.65)
    )
)

(define (beside painter1 painter2)
    (let ((split-vect (make-vect 0.5 0.0)))
        (let ((paint-left (transform-painter painter1 (make-vect 0.0 0.0) split-vect (make-vect 0.0 1.0)))
              (paint-right (transform-painter painter2 split-vect (make-vect 1.0 0.0) (make-vect 0.5 1.0)))
             )
            (lambda (frame)
                (paint-left frame)
                (paint-right frame)
            )
        )
    )
)
