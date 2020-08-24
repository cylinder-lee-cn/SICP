#lang racket
(#%require "../cal-common.scm")

(install-rectangular-package)
(install-polar-package)
(install-scheme-number-package)
(install-rational-package)
(install-realnumber-package)
(install-complex-package)
(install-coercion-raise-package)
(install-coercion-drop-package)

;; polynomial
(define (adjoin-term term term-list)
    (if (=zero? (coeff term))
        term-list
        (cons term term-list)
    )
)
(define (the-empty-termlist) `())
(define (first-term term-list) (car term-list))
(define (rest-term term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))
(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
            (let ((t1 (first-term L1)) (t2 (first-term L2)))
                (cond ((> (order t1) (order t2))
                        (adjoin-term t1 (add-terms (rest-term L1) L2))
                      )
                      ((< (order t1) (order t2))
                        (adjoin-term t2 (add-terms L1 (rest-term L2)))
                      )
                      (else
                        (adjoin-term
                            (make-term (order t1)
                                       (add (coeff t1) (coeff t2))
                            )
                            (add-terms (rest-term L1) (rest-term L2))
                        )
                      )

                )
            )
          )
    )
)

(define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-term L1) L2)
        )
    )
)

(define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
            (adjoin-term
                (make-term (+ (order t1) (order t2)) (mul (coeff t1) (coeff t2)))
                (mul-term-by-all-terms t1 (rest-term L))
            )
        )
    )
)

(define (terms-zero? terms)
    (if (empty-termlist? terms)
        true
        (and (=zero? (coeff (first-term terms)))
             (terms-zero? (rest-term terms))
        )
    )
)

(define (install-polynomial-package)
    ;; internal procdures
    ;; representation of poly
    (define (make-poly variable term-list)
        (cons variable term-list)
    )
    (define (variable p) (car p))
    (define (term-list p) (cdr p))

    ;; representation of terms and term lists
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2))
            )
            (error "Polys not in same var -- ADD-POLY" (list p1 p2))
        )
    )
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2))
            )
            (error "Polys not in same var -- MUL-POLY" (list p1 p2))
        )
    )

    (define (poly-zero? p) (terms-zero? (term-list p)))

    ;; interface to rest of the system
    (define (tag p) (attach-tag `polynomial p))
    (put `add `(polynomial polynomial)
        (lambda (p1 p2) (tag (add-poly p1 p2)))
    )
    (put `mul `(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2)))
    )
    (put `make `polynomial
        (lambda (var terms) (tag (make-poly var terms)))
    )
    (put `=zero? `(polynomial)
        (lambda (p) (poly-zero? p))
    )
    `polynomial-done
)

(define (make-polynomial var terms)
    ((get `make `polynomial) var terms)
)

(install-polynomial-package)

(define t0 (list (make-term 4 0) (make-term 2 0) (make-term 0 0)))
(define t1 (list (make-term 2 5) (make-term 1 3) (make-term 0 7)))
(define t2 (list (make-term 2 7) (make-term 1 4) (make-term 0 9)))
(define p0 (make-polynomial `x t0))
(define p1 (make-polynomial `x t1))
(define p2 (make-polynomial `x t2))

(add p1 p2)
(mul p1 p2)
