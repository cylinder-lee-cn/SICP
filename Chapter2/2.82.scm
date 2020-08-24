#lang racket
(#%require "../racket-common.scm")

#|
Show how to generalize apply-generic to handle coercion in the general case of 
multiple arguments. One strategy is to attempt to coerce all the arguments to 
the type of the first argument, then to the type of the second argument, and 
so on. 
Give an example of a situation where this strategy (and likewise the 
two-argument version given above) is not sufficiently general. 
(Hint: Consider the case where there are some suitable mixed-type operations 
present in the table that will not be tried.)

在2.81的基础上改造apply-generic，coercion类型能支持多个，不仅仅是2个。
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
        (make-from-real-imag (add (real-part z1) (real-part z2))
                             (add (imag-part z1) (imag-part z2))
        )
    )
    (define (sub-complex z1 z2)
        (make-from-real-imag (sub (real-part z1) (real-part z2))
                             (sub (imag-part z1) (imag-part z2))
        )
    )
    (define (mul-complex z1 z2)
        (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                           (add (angle z1) (angle z2))
        )
    )
    (define (div-complex z1 z2)
        (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                           (sub (angle z1) (angle z2))
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
        (make-complex-from-real-imag n 0)
    )
    (define (scheme-number->rational n)
        (make-rational n 1)
    )
    (define (rational->complex n)
        (make-complex-from-real-imag n 0)
    )
    ; (define (scheme-number->scheme-number n) n)
    ; (define (rational->rational r) r)
    ; (define (complex->complex z) z)
    ;;interface
    (put-coercion `scheme-number `complex scheme-number->complex)
    (put-coercion `scheme-number `rational scheme-number->rational)
    (put-coercion `rational `complex rational->complex)
    ; (put-coercion `scheme-number `scheme-number scheme-number->scheme-number)
    ; (put-coercion `rational `rational rational->rational)
    ; (put-coercion `complex `complex complex->complex)

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

(define i1 (make-scheme-number 7))
(define r1 (make-rational 2 3))
(define r2 (make-rational 4 7))
(define z1 (make-complex-from-real-imag (make-rational 1 9) 5))
(define z2 (make-complex-from-real-imag 2 6))

i1
r1
r2
z1
z2
(add r1 z1)

#|
1. 要重新设计类型转换的过程，使其能够接收多个参数，并且按照多个参数中类型进行全部类型的组合
需要使用2个map，此处的args已经是所有参数的list，如果有3个参数 int rat complex
组合出来就是 
((int int) (rat int) (complex int)
(int rat) (rat rat) (complex rat)
(int complex) (rat complex) (complex complex))
那查表的结果会有(假设已经存在rational->complex)
(
    (proc) (#f) (#f)
    (proc) (proc) (#f)
    (proc) (proc) (proc)
)

|#
(define (coercion-proc args)
    (display args)
    (newline)
    (map (lambda (a) (let ((type1 (type-tag a)))
                        (map (lambda (b)
                                (let ((type2 (type-tag b)))
                                    (if (eq? type1 type2)
                                        (lambda (x) x)
                                        (get-coercion type2 type1)
                                    )
                                )
                             )
                             args
                        )
                     )
         )
         args
    )
)
#|
2. 根据上述查找出来的过程list，过滤掉含有#f的
那就是
(int complex) (rat complex) (complex complex)
对应的类型转换过程
(proc) (proc) (proc)
|#
(define (coercion-proc-list procs-list)
    (filter (lambda (p) (not (memq #f p))) procs-list)
)
#|
3. 把类型转换过程对应的使用到每个对应的参数上
|#
(define (coercion-each-args procs-list args)
    (map (lambda (f x) (f x))
        procs-list
        args
    )
)

;;操作proc列表应用到参数列表的测试
(coercion-each-args (list abs sqrt square) (list -9 4 7))

(define test-args (list i1 r1 z2))
(coercion-each-args (car (coercion-proc-list (coercion-proc test-args))) test-args)