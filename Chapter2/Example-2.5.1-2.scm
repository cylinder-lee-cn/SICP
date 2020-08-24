#lang sicp

;;常规计算
(define (add x y) (apply-generic `add x y))
(define (sub x y) (apply-generic `sub x y))
(define (mul x y) (apply-generic `mul x y))
(define (div x y) (apply-generic `div x y))

(define (install-scheme-number-package)
    (define (tag x) (attach-tag `scheme-number x))
    (put `add `(scheme-number scheme-number) (lambda (x y) (tag (+ x y))))
    (put `sub `(scheme-number scheme-number) (lambda (x y) (tag (- x y))))
    (put `mul `(scheme-number scheme-number) (lambda (x y) (tag (* x y))))
    (put `div `(scheme-number scheme-number) (lambda (x y) (tag (/ x y))))
    (put `make `scheme-number (lambda (x) (tag x)))
    `done
)

(define (make-scheme-number n) ((get `make `scheme-number) n))


;;有理数计算
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
        (make-rat (* (numer x) (numer y)) (* (denom x) (denom y)))
    )
    (define (div-rat x y)
        (make-rat (* (numer) (denom y)) (* (denom x) (numer y)))
    )
    ;;interface to rest of the system
    (define (tag x) (attach-tag `rational x))
    (put `add `(rational rational) (lambda (x y) (tag (add-rat x y))))
    (put `sub `(rational rational) (lambda (x y) (tag (sub-rat x y))))
    (put `mul `(rational rational) (lambda (x y) (tag (mul-rat x y))))
    (put `div `(rational rational) (lambda (x y) (tag (div-rat x y))))
    (put `make `rational (lambda (n d) (tag (make-rat n d))))
    `done
)

(define (make-rational n d) ((get `make `rational) n d))

;;复数计算
(define (install-complex-package)
    ;;imported procedures from rectangular and polar packages
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
    ;;interface ot rest of system
    (define (tag z) (attach-tag `complex z))
    (put `add `(complex complex) (lambda (x y) (tag (add-complex z1 z2))))
    (put `sub `(complex complex) (lambda (x y) (tag (sub-complex z1 z2))))
    (put `mul `(complex complex) (lambda (x y) (tag (mul-complex z1 z2))))
    (put `div `(complex complex) (lambda (x y) (tag (div-complex z1 z2))))
    (put `make-from-real-imag `complex
        (lambda (x y) (tag (make-from-real-imag x y)))
    )
    (put `make-from-mag-ang `complex
        (lambda (x y) (tag (make-from-mag-ang x y)))
    )
    `done
)

(define (make-complex-from-real-imag x y)
    ((get `make-from-real-imag `complex) x y)
)
(define (make-complex-from-mag-ang x y)
    ((get `make-from-mag-ang `complex) x y)
)

;;2.5.2 不同类型数据的组合

;;to be included in the complex package
(define (add-complex-to-schemenum z x)
    (make-from-real-imag (+ (real-part z) x) (imag-part z))
)
(put `add `(complex scheme-number)
    (lambda (z x) (tag (add-complex-to-schemenum z x)))
)
#|
可以为每种不同类型数据的计算进行组合，从而定义对应的计算方法。可行，但是非常麻烦。
使用类型转换，将常规数值转换为虚部为0的复数，那就成了复数对复数的计算
|#
(define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0)
)
;;假定有操作表格的put-coercion和get-coercion过程
(put-coercion `scheme-number `complex scheme-number->complex)

#|
首先检查是否存在针对实际参数的操作定义，如果存在，那就正常执行。
否则就做强制的类型转换，（只考虑两个参数的情况，为了简化问题），先看第一个参数是否能转换到第二
个，如果不行就看第二个能否转换到第一个，如果都不行就放弃，报错。
|#
(define (apply-generic op . args)
    (let ((type-tags (map type-tag args)))
        (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (if (= (length args 2))
                    (let ((type1 (car type-tags))
                          (type2 (cadr type-tags))
                          (a1 (car args))
                          (a2 (cadr args))
                         )
                         (let ((t1->t2 (get-coercion type1 type2))
                               (t2->t1 (get-coercion type2 type1))
                              )
                              (cond ((t1->t2) (apply-generic op (t1->t2 a1) a2))
                                    ((t2->t1) (apply-generic op a1 (t2->t1 a2)))
                                    (else (error "No method for these types"))
                              )

                         )
                    )
                    (error "No method for these types")
                )
            )
        )
    )
)

