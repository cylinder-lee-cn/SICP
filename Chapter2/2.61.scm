#lang sicp

(define (element-of-set? x set)
    (cond ((null? set) false)
          ((= x (car set)) true)
          ((< x (car set)) false)
          (else
            (element-of-set? x (cdr set))
          )
    )
)

(define (intersection-set set1 set2)
    (if (or (null? set1) (null? set2))
        `()
        (let ((x1 (car set1))
              (x2 (car set2))
             )
             (cond ((= x1 x2)
                    (cons x1 (intersection-set (cdr set1) (cdr set2)))
                   )
                   ((< x1 x2)
                    (intersection-set (cdr set1) set2)
                   )
                   ((> x1 x2)
                    (intersection-set set1 (cdr set2))
                   )
             )
        )
    )
)

; (define (adjoin x set)
;     (define (join-iter n s result)
;         (if (null? s)
;             (append result (list x))
;             (let ((t (car s)))
;                 (cond ((< t x) (join-iter n (cdr s) (append result (list t))))
;                       ((= t x) set)
;                       ((> t x) (append (append result (list x)) s))
;                 )
;             )
;         )
        
;     )
;     (if (null? set)
;         (list x)
;         (join-iter x set `())
;     )
; )

; (define (adjoin1 x set)
;     (define (join-iter n s result)
;         (if (null? s)
;             (cons x result)
;             (let ((t (car s)))
;                 (cond ((< t x) (join-iter n (cdr s) (cons t result)))
;                       ((= t x) set)
;                       ((> t x)  (cons (cons x result) s))
;                 )
;             )
;         )
        
;     )
;     (if (null? set)
;         (list x)
;         (join-iter x set `())
;     )
; )

(define (adjoin x set)
    (if (null? set)
        (list x)
        (let ((head (car set))
              (tail (cdr set))
             )
             (cond ((= x head) set)
                   ((> x head) (cons head (adjoin x tail)))
                   ((< x head) (cons x set))
             )
        )
    )
)


(define aset `(1 2 3 4 5))
(define bset `(3 4 5 6 7))
(define cset `(1 3 6 10))

(intersection-set aset bset)

(adjoin 3 cset)