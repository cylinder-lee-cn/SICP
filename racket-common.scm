#lang racket
#|
Use put & get must be `racket`
|#

(define nil `())

(define (square x) (* x x))

;; for list

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence))
           (cons (car sequence) (filter predicate (cdr sequence)))
          )
          (else (filter predicate (cdr sequence)))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence))
        )
    )
)

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))
    )
)

;;
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2))
)
(define (=number? exp num) (and (number? exp) (= exp num)))

;; put & get
(define *op-table* (make-hash))
(define (put op type proc)
    (hash-set! *op-table* (list op type) proc)
)
(define (get op type)
    (hash-ref *op-table* (list op type) #f)
)

;;

(define (put-coercion type1 type2 proc)
    (hash-set! *op-table* (list type1 type2) proc)
)
(define (get-coercion type1 type2)
    (hash-ref *op-table* (list type1 type2) #f)
)

;;tag content
(define (attach-tag type-tag contents)
    (if (number? contents)
        contents
        (cons type-tag contents)
    )
)
(define (type-tag datum)
    (cond ((and (integer? datum) (= datum 0)) `realnumber)
          ((and (integer? datum) (exact? datum)) `scheme-number)
          ((and (real? datum) (inexact? datum)) `realnumber)
          ((pair? datum) (car datum))
          (else (error "Bad tagged datum -- TYPE-TAG" datum))
    )
)
(define (contents datum)
    (cond ((number? datum) datum)
          ((pair? datum) (cdr datum))
          (else (error "Bad tagged datum -- CONTENTS" datum))
    )
)
;;apply-generic 
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error "No method for these types" (list op type-tags))
            )
        )
    )
)
;;
(provide (all-defined-out))
