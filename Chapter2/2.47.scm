#lang sicp

;first implement
(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2)
)


(define (origin-frame frame)
    (car frame)
)

(define (edge1-frame frame)
    (cadr frame)
)

(define (edge2-frame frame)
    (caddr frame)
)

;second implement
(define (make-frame1 origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (origin-frame1 frame)
    (car frame)
)

(define (edge1-frame1 frame)
    (cadr frame)
)

(define (edge2-frame1 frame)
    (cddr frame)
)

