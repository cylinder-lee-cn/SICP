#lang sicp
#|
根据书中给出的关系 (bn/2)2=(b2)n/2 ，并且使用一个不变量记录中间结果，写出对数步数内迭代计算幂的函数
|#
(define (square x) (* x x))

(define (fast-expt b n) (expt-iter b n 1))

(define (expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n)  (expt-iter (square b) (/ n 2) a))
          ((odd? n) (expt-iter b (- n 1) (* b a)))
    )
)

(display (fast-expt 2 7))

(exit)


#|

def f(b n a)
        if n ==0 :
            return a
        elif n==even:
            f(b^2 n/2 a)
        elif n==odd:
            f(b n-1 a*b)
#|