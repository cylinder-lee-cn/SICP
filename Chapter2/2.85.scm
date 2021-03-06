#lang racket
(#%require "../racket-common.scm")
#|

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
    (put `equ? `(rectangular polar) equ?)
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
    (put `equ? `(polar rectangular) equ?)
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
    (put `numer `(rational) numer)
    (put `denom `(rational) denom)
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
(define (numer x) ((get `numer `(rational)) x))
(define (denom x) ((get `denom `(rational)) x))

;;real-number
(define (install-realnumber-package)
    ;;interface
    (define (tag x) (attach-tag `realnumber x))
    (put `equ? `(realnumber realnumber) =)
    (put `=zero? `(realnumber) zero?)
    (put `add `(realnumber realnumber)
        (lambda (x y) (tag (+ x y)))
    )
    (put `sub `(realnumber realnumber)
        (lambda (x y) (tag (- x y)))
    )
    (put `mul `(realnumber realnumber)
        (lambda (x y) (tag (* x y)))
    )
    (put `div `(realnumber realnumber)
        (lambda (x y) (tag (/ x y)))
    )
    (put `exp `(realnumber realnumber)
        (lambda (x y) (tag (expt x y)))
    )
    (put `make `realnumber (lambda (x) (tag x)))
    `real-done    
)
(define (make-real-number n)
    ((get `make `realnumber) n)
)

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

;;定义类型转换 提升---coercion--raise
(define (install-coercion-raise-package)
    ;;procedure
    (define (scheme-number->rational n)
        (make-rational (contents n) 1)
    )
    (define (rational->realnumber n)
        (let ((nc (contents n)))
            (make-real-number (* 1.0 (/ (numer nc) (denom nc))))
        )
    )
    (define (realnumber->complex n)
        (make-complex-from-real-imag (contents n) 0)
    )
    (define (complex->complex n) n)

    ;;interface
    (put-coercion `raise `scheme-number scheme-number->rational)
    (put-coercion `raise `rational rational->realnumber)
    (put-coercion `raise `realnumber realnumber->complex)
    (put-coercion `raise `complex complex->complex)

    `coercion-raise-done
)

;;定义类型转换 下降 ---coercion--drop
(define (install-coercion-drop-package)
    ;;procedure
    (define (complex->number n)
        (let ((realproject (make-real-number (real-part n))))
            (let ((projectraise (raise realproject)))
                (if (equ? n projectraise)
                    realproject
                    n
                )
            )
        )
    )
    (define (realnumber->integer n)
        (let ((intproject (round n)))
            (let ((projectraise (raise intproject)))
                (if (equ? n projectraise)
                    intproject
                    n
                )
            )
        )
    )
    (define (rational->integer n)
        (let ((intproject (numer (contents n))))
            (let ((projectraise (raise intproject)))
                (if (equ? n projectraise)
                    intproject
                    n
                )
            )
        )
    )
    (define (integer->integer n) n)
    (put-coercion `drop `complex complex->number)
    (put-coercion `drop `realnumber realnumber->integer)
    (put-coercion `drop `rational rational->integer)
    (put-coercion `drop `scheme-number integer->integer)
)

;;定义最外层的操作
(define (add a b) (apply-generic `add a b))
(define (sub a b) (apply-generic `sub a b))
(define (mul a b) (apply-generic `mul a b))
(define (div a b) (apply-generic `div a b))
(define (equ? x y) (apply-generic `equ? x y))
(define (=zero? x) (apply-generic `=zero? x))
(define (exp x y) (apply-generic `exp x y))
(define (raise n) ((get-coercion `raise (type-tag n)) n))
(define (drop n) ((get-coercion `drop (type-tag n)) n))
;;
(install-scheme-number-package)
(install-rational-package)
(install-realnumber-package)
(install-complex-package)
(install-coercion-raise-package)
(install-coercion-drop-package)
(newline)

(define (n-level t)
    (cond ((eq? `scheme-number t) 0)
          ((eq? `rational t) 1)
          ((eq? `realnumber t) 2)
          ((eq? `complex t) 3)
    )
)

;按照只有2个参数来设计
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (if (or (eq? op `equ?) (eq? op `raise) (eq? op `=zero?) (eq? op `drop))
                    (apply proc (map contents args))
                    (drop (apply proc (map contents args)))
                )
                (let ((l1 (n-level (car type-tags)))
                      (l2 (n-level (cadr type-tags)))
                      (v1 (car args))
                      (v2 (cadr args))
                     )
                     (cond ((> l1 l2) (apply-generic op v1 (raise v2)))
                           ((< l1 l2) (apply-generic op (raise v1) v2))
                           (else (error "No method for these args"))
                     )
                )
            )
        )
    )
)

(define zr1 (make-complex-from-real-imag 2 0))
(define zr2 (make-complex-from-real-imag 2.0 1.0))
(define zp1 (make-complex-from-mag-ang 9 1))
(define zp2 (make-complex-from-mag-ang 9.0 1.0))
(define r1 (make-real-number 1.0))
(define r2 (make-real-number 2))
(define rat1 (make-rational 3 1))
(define rat2 (make-rational 4 9))
(define i1 (make-scheme-number 5))
(define i2 (make-scheme-number 7))
i1
i2
rat1
rat2
r1
r2
zr1
zr2
zp1
zp2
(display "----------------------")
(newline)

(add 3 zr1)
