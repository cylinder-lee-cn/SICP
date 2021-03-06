#lang sicp

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp)
            (if (same-variable? exp var) 1 0)
          )
          ((sum? exp)
            (make-sum (deriv (addend exp) var)
                      (deriv (augend exp) var)
            )
          )
          ((product? exp)
            (make-sum (make-product (multiplier exp) (deriv (multiplicand exp) var))
                      (make-product (deriv (multiplier exp) var) (multiplicand exp))
            )
          )
          ((exponentiation? exp)
            (let ((b (base exp))
                  (e (exponent exp))
                 )
                 (make-product (make-product e (make-exponentiation b (- e 1)))
                               (deriv b var)
                 )
            )
          )
          (else (error "Unkown expression type."))
    )
)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)

(define (=number? e n)
    (and (number? e) (= e n))
)

(define (make-sum a1 a2)
    ; (list `+ a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else
            (list `+ a1 a2)
          )
    )
)

(define (make-product m1 m2)
    ; (list `* m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else
            (list `* m1 m2)
          )
    )
)

(define (sum? x)
    (and (pair? x) (eq? (car x) `+))
)
(define (addend s)
    (cadr s)
)
(define (augend s)
    (caddr s)
)
(define (product? s)
    (and (pair? s) (eq? (car s) `*))
)
(define (exponentiation? s)
    (and (pair? s) (eq? (car s) `**))
)
(define (multiplier s)
    (cadr s)
)
(define (multiplicand s)
    (caddr s)
)

(define (make-exponentiation base exponent)
    ; (list `** base exponent)
    (cond ((=number? exponent 0) 1)
          ((=number? exponent 1) base)
          (else
            (list `** base exponent)
          )
    )
)
(define (base s)
    (cadr s)
)
(define (exponent s)
    (caddr s)
)
(define (make-sub a b)
    (list `- a b)
)
; (deriv `(** u 4) `u)

(deriv `(+ x 3 x) `x)
