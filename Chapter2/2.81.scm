#lang racket
(#%require "../racket-common.scm")

#|
Louis Reasoner has noticed that apply-generic may try to coerce the arguments to
 each other's type even if they already have the same type. Therefore, 
 he reasons, we need to put procedures in the coercion table to "coerce" 
 arguments of each type to their own type. For example, in addition to the 
 scheme-number->complex coercion shown above, he would do:

(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)
(put-coercion 'complex 'complex complex->complex)

a. With Louis's coercion procedures installed, what happens if apply-generic is 
called with two arguments of type scheme-number or two arguments of type complex 
for an operation that is not found in the table for those types? For example, 
assume that we've defined a generic exponentiation operation:

(define (exp x y) (apply-generic 'exp x y))

and have put a procedure for exponentiation in the Scheme-number package but not 
in any other package:

;; following added to Scheme-number package
(put 'exp '(scheme-number scheme-number)
     (lambda (x y) (tag (expt x y)))) ; using primitive expt

What happens if we call exp with two complex numbers as arguments?

b. Is Louis correct that something had to be done about coercion with arguments 
of the same type, or does apply-generic work correctly as is?

c. Modify apply-generic so that it doesn't try coercion if the two arguments 
have the same type.
|#
#|
通用型算术运算，通用算术包
|#
;;以下是复数运算包
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
    (define (equ? z1 z2)
        (and (= (real-part z1) (real-part z2)) (= (imag-part z2) (imag-part z1)))
    )
    (define (=zero? z)
        (and (zero? (real-part z)) (zero? (imag-part z)))
    )
    ;;interface to the rest of system
    (define (tag x) (attach-tag `rectangular x))
    (put `equ? `(rectangular rectangular) equ?)
    (put `=zero? `(rectangular) =zero?)

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
    `rectangular-done
)
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
    (define (equ? z1 z2)
        (and (= (magnitude z1) (magnitude z2)) (= (angle z1) (angle z2)))
    )
    (define (=zero? z) (zero? (magnitude z))
    )
    ;;interface to the rest of the system
    (define (tag x) (attach-tag `polar x))
    (put `equ? `(polar polar) equ?)
    (put `=zero? `(polar) =zero?)

    (put `real-part `(polar) real-part)
    (put `imag-part `(polar) imag-part)
    (put `magnitude `(polar) magnitude)
    (put `angle `(polar) angle)
    (put `make-from-real-imag `polar
         (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put `make-from-mag-ang `polar
         (lambda (r a) (tag (make-from-mag-ang r a)))
    )
    `polar-done
)
(newline)
(install-rectangular-package)
(install-polar-package)

;;integer
(define (install-scheme-number-package)
    ;;interface
    (define (tag x) (attach-tag `scheme-number x))
    (put `equ? `(scheme-number scheme-number) =)
    (put `=zero? `(scheme-number) zero?)
    (put `add `(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y)))
    )
    (put `sub `(scheme-number scheme-number)
        (lambda (x y) (tag (- x y)))
    )
    (put `mul `(scheme-number scheme-number)
        (lambda (x y) (tag (* x y)))
    )
    (put `div `(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y)))
    )
    (put `exp `(scheme-number scheme-number)
        (lambda (x y) (tag (expt x y)))
    )
    (put `make `scheme-number (lambda (x) (tag x)))
    `integer-done
)
(define (make-scheme-number n)
    ((get `make `scheme-number) n)
)

;;rational
(define (install-rational-package)
    ;;internal procedures
    (define (numer x) (car x))
    (define (denom x) (cdr x))
    (define (make-rat n d)
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))
        )
    )
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
                  (* (denom x) (denom y))
        )
    )
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
                  (* (denom x) (denom y))
        )    
    )
    (define (mul-rat x y)
        (make-rat (* (numer x) (* numer y)) (* (denom x) (denom y)))
    )
    (define (div-rat x y)
        (make-rat (* (numer x) (* denom y)) (* (denom x) (numer y)))
    )
    (define (equ? x y)
        (and (= (numer x) (numer y)) (= (denom x) (denom y)))
    )
    (define (=zero? x)
        (zero? (numer x))
    )
    ;;interface to rest of the system
    (define (tag x) (attach-tag `rational x))
    (put `equ? `(rational rational) equ?)
    (put `=zero? `(rational) =zero?)
    (put `add `(rational rational)
        (lambda (x y) (tag (add-rat x y)))
    )
    (put `sub `(rational rational)
        (lambda (x y) (tag (sub-rat x y)))
    )
    (put `mul `(rational rational)
        (lambda (x y) (tag (mul-rat x y)))
    )
    (put `div `(rational rational)
        (lambda (x y) (tag (div-rat x y)))
    )
    (put `make `rational (lambda (n d) (tag (make-rat n d))))
    `rational-done
)
(define (make-rational n d) ((get `make `rational) n d))

;;complex
(define (install-complex-package)
    ;;imported procedures from rectangular and polar package
    (define (make-from-real-imag x y)
        ((get `make-from-real-imag `rectangular) x y)
    )
    (define (make-from-mag-ang r a)
        ((get `make-from-mag-ang `polar) r a)
    )
    ;;internal procedures
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
    ;;interface to rest of the system
    (define (tag z) (attach-tag `complex z))
    ;;
    (put `equ? `(complex complex) equ?)
    (put `=zero? `(complex) =zero?)
    ;;
    (put `real-part `(complex) real-part)
    (put `imag-part `(complex) imag-part)
    (put `magnitude `(complex) magnitude)
    (put `angle `(complex) angle)
    ;;
    (put `add `(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put `sub `(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put `mul `(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put `div `(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put `make-from-real-imag `complex
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put `make-from-mag-ang `complex
        (lambda (r a) (tag (make-from-mag-ang r a)))
    )
    `complex-done
)

;;定义类型转换---coercion
(define (install-coercion-package)
    ;;procedure
    (define (scheme-number->complex n)
        (make-complex-from-real-imag (contents n) 0)
    )
    (define (scheme-number->scheme-number n) n)
    (define (complex->complex z) z)
    ;;interface
    (put-coercion `scheme-number `complex scheme-number->complex)
    (put-coercion `scheme-number `scheme-number scheme-number->scheme-number)
    (put-coercion `complex `complex complex->complex)

    `coercion-done
)

;;定义最外层的操作
(define (make-complex-from-real-imag x y)
    ((get `make-from-real-imag `complex) x y)
)
(define (make-complex-from-mag-ang r a)
    ((get `make-from-mag-ang `complex) r a)
)
(define (real-part z) (apply-generic `real-part z))
(define (imag-part z) (apply-generic `imag-part z))
(define (magnitude z) (apply-generic `magnitude z))
(define (angle z) (apply-generic `angle z))
(define (add a b) (apply-generic `add a b))
(define (sub a b) (apply-generic `sub a b))
(define (mul a b) (apply-generic `mul a b))
(define (div a b) (apply-generic `div a b))
(define (equ? x y) (apply-generic `equ? x y))
(define (=zero? x) (apply-generic `=zero? x))
(define (exp x y) (apply-generic `exp x y))
;;
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)
(install-coercion-package)
(newline)

(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= 2 (length args))
                    (let ((t1 (car type-tags))
                          (t2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args))
                         )
                         (if (eqv? t1 t2)
                            (error "No method for these types" type-tags)
                            (let ((t1->t2 (get-coercion t1 t2))
                                  (t2->t1 (get-coercion t2 t1))
                                 )
                                (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                                        (t2->t1 (apply-generic op (t2->t1 a2) a1))
                                        (else (error "No method for these types"))
                                )
                            )

                         )
                    )
                    (error "No method for these types" (list op type-tags))
                )
            )
        )
    )
)

(define z1 (make-complex-from-real-imag 3 4))
(define z2 (make-complex-from-real-imag 1 2))

(add z1 4)

(exp 2 4)
(exp z1 z2)

#|
a:
如果用两个复数调用
    (exp z1 z2)
=>   (apply-generic `exp z1 z2)
=>   ... 会执行到 (if proc) 在复数package中找不到exp
=>   会执行到类型判断
=>   (let t1->t2 ...) 由于存在(complex->complex) 所以就会执行到
=>   (apply-generic `exp (complex->complex z1) z2)
=>   (apply-generic `exp z1 z2)
就成了死循环

b:
没解决问题，apply-generic不能正常工作

c:
|#

