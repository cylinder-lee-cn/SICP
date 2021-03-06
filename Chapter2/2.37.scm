#lang sicp

(define mx (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        nil
        (cons (accumulate op init (map car seqs))
              (accumulate-n op init (map cdr seqs))
        )
    )
)

(define (dot-product v w)
    (accumulate + 0 (map * v w))
)

(dot-product (list 1 2 3) (list 4 5 6))

(define (transpose mat)
    (accumulate-n cons nil mat)
)

(transpose mx)

(define (matrix-*-vector m v)
    (map (lambda (x) (dot-product v x)) m)
)

(matrix-*-vector mx (list 1 2 3 4))

(define (matrix-*-matrix m n)
    (let ((cols (transpose n)))
        (map (lambda (v) (matrix-*-vector cols v)) m)
    )
)


(matrix-*-matrix mx (transpose mx))
