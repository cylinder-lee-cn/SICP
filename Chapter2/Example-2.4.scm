#lang sicp

#|
2.4.1 复数的表示
|#

; (make-from-real-imag (real-part z) (imag-part z))
; (make-from-mag-ang (magnitude z) (angle z))

(define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))
    )
)

(define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))
    )
)

(define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))
    )
)

(define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))
    )
)

#|
x = r cos(A)
y = r sin(A)
r = sqrt(x^2+y^2)
A = arctan(y,x)
|#

#|
Ben 直角座标方式
|#

(define (square x) (* x x))
(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
    (sqrt (+ (square (real-part z)) (square (imag-part z))))
)
(define (angle z)
    (atan (imag-part z) (real-part z))
)

(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a)))
)

#|
Alyssa 极座标方式
|#

(define (real-part z)
    (* (magnitude z) (cos (angle z)))
)
(define (imag-part z)
    (* (magnitude z) (sin (angle z)))
)
(define (magnitude z) (car z))
(define (angle z) (cdr z))
(define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y))) (atan y x))
)
(define (make-from-mag-ang r a) (cons r a))

#|
2.4.2 带标志数据
|#

(define (attach-tag type-tag contents)
    (cons type-tag contents)
)
(define (type-tag datum)
    (if (pair? datum)
        (car datum)
        (error "Bad tagged datum -- TYPE-TAG" datum)
    )
)
(define (contents datum)
    (if (pair? datum)
        (cdr datum)
        (error "Bad tagged datum -- CONTENTS" datum)
    )
)
(define (rectangular? z)
    (eqv? (type-tag z) `rectangular)
)
(define (polar? z)
    (eqv? (type-tag z) `polar)
)

; Ben
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
    (sqrt (+ (square (real-part-rectangular z)) 
             (square (imag-part-rectangular z))))
)
(define (angle-rectangular z)
    (atan (imag-part-rectangular z) (real-part-rectangular z))
)
(define (make-from-real-imag-rectangular x y)
    (attach-tag `rectangular (cons x y))
)
(define (make-from-mag-ang-rectangular r a)
    (attach-tag `rectangular 
                (cons (* r (cos a)) (* r (sin a)))
    )
)
; Alyssa
(define (real-part-polar z)
    (* (magnitude-polar z) (cos (angle-polar z)))
)
(define (imag-part-polar z)
    (* (magnitude-polar z) (sin (angle-polar z)))
)
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
    (attach-tag `polar 
                (cons (sqrt (+ (square x) (square y))) (atan y x))
    )
)
(define (make-from-mag-ang-polar r a)
    (attach-tag `polar (cons r a))
)
(define (real-part z)
    (cond ((rectangular? z) (real-part-rectangular (contents z)))
          ((polar? z) (real-part-polar (contents z)))
          (else (error "Unkown type -- REAL-PART" z))
    )
)
(define (imag-part z)
    (cond ((rectangular? z) (imag-part-rectangular (contents z)))
          ((polar? z) (imag-part-polar (contents z)))
          (else (error "Unkown type -- IMAG-PART" z))
    )
)
(define (magnitude z)
    (cond ((rectangular? z) (magnitude-rectangular (contents z)))
          ((polar? z) (magnitude-polar (contents z)))
          (else (error "Unkown type -- MAGNITUDE" z))
    )
)
(define (angle z)
    (cond ((rectangular? z) (angle-rectangular (contents z)))
          ((polar? z) (angle-polar (contents z)))
          (else (error "Unkown type -- ANGLE" z))
    )
)

(define (make-from-real-imag x y)
    (make-from-real-imag-rectangular x y)
)
(define (make-from-mag-ang r a)
    (make-from-mag-ang-polar r a)
)

#|
2.4.3 数据导向的程序设计和可加性
|#

;;Ben
(define (install-rectangular-package)
    ;;internal procedures
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (make-from-real-imag x y) (cons x y))
    (define (magnitude z)
        (sqrt (+ (square (real-part z)) (square (imag-part z))))
    )
    (define (angle z)
        (atan (imag-part z) (real-part z))
    )
    (define (make-from-mag-ang r a)
        (cons (* r (cos a)) (* r (sin a)))
    )
    ;;interface to the rest of system
    (define (tag x) (attach-tag `rectangular x))
    (put `real-part `(rectangular) real-part)
    (put `imag-part `(rectangular) imag-part)
    (put `magnitude `(rectangular) magnitude)
    (put `angle `(rectangular) angle) 
    (put `make-from-real-imag `rectangular 
         (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put `make-from-mag-ang `rectangular
         (lambda (r a) (tag (make-from-mag-ang r a)))
    )
    `done
)

;;Alyssa
(define (install-polar-package)
    ;;internal procedures
    (define (magnitude z) (car z))
    (define (angle z) (cdr z))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z)
        (* (magnitude z) (cos (angle z)))
    )
    (define (imag-part z)
        (* (magnitude z) (sin (angle z)))
    )
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y))) (atan y x))
    )
    ;;interface to the rest of the system
    (define (tag x) (attach-tag `polar x))
    (put `real-part `(polar) real-part)
    (put `imag-part `(polar) imag-part)
    (put `magnitude `(polar) magnitude)
    (put `angle `(polar) angle)
    (put `make-from-real-imag `polar
         (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put `make-from-mag-ang `polar
         (lambda (r a) (tag (make-from-man-ang r a)))
    )
    `done
)

;;
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
#|
这个分别定义的real-part...其实和`消息传递`中定义的方式正好是倒过来的
|#
(define (real-part z) (apply-generic `real-part z))
(define (imag-part z) (apply-generic `imag-part z))
(define (magnitude z) (apply-generic `magnitude z))
(define (angle z) (apply-generic `angle z))

(define (make-from-real-imag x y)
    ((get `make-from-real-imag `rectangular) x y)
)
(define (make-from-mag-ang r a)
    ((get `make-from-mag-ang `polar) r a)
)

;;消息传递

(define (make-from-real-imag x y)
    (define (dispatch op)
        (cond ((eqv? op `real-part) x)
              ((eqv? op `imag-part) y)
              ((eqv? op `magnitude) 
                (sqrt (+ (square x) (square y)))
              )
              ((eqv? op `angle) (atan y x))
              (else (error "Unkown op" op))
        )
    )
    dispatch
)

(define (apply-generic op arg) (arg op))

#|
由于make-from-real-imag其实是返回一个过程（dispatch），这个过程有一个op的参数。
那么apply-generic的调用实际上是：
  (apply-generic `real-part (make-from-real-imag x y))
=>((make-from-real-imag x y) `real-part)
=>(dispatch `real-part)
=>x

make-from-real-imag 没有真正的将x y 组合到一起，而且通过内部的dispatch过程来决定
如何返回不同的x y

apply-generic其实是再调用make-from-real-imag的dispatch
|#
