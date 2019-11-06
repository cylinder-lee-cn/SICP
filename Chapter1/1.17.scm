#lang sicp
#|
首先写出 double 和 halve 两个辅助函数，其中 double 求出一个数的两倍，而 halve 则将一个数除以 2 

然后利用类似书本 30 页的 fast-expt 的技术，写出使用对数步数求乘积的函数（为了和内置的 * 函数区分开，函数使用 multi 作为名字）
|#

(define (double x) (+ x x))

(define (halve x) (/ x 2))

(define (multiply a b)
    (cond ((= b 0) 0)
          ((even? b) (double (multiply a (halve b))))
          ((odd? b) (+ a (multiply a (- b 1))))
    )
)

(double 2)
(newline)
(halve 4)
(newline)
(multiply 41 32)




#|
def double(x):
    return x+x

def halve(x):
    return x/2

def multiply(a,b):
    if b==0:
        return 0
    elif b==even:
        double(multiply(a,halve(b)))
    elif b==odd:
        a+multiply(a,b-1)
|#