#lang sicp
#|
函数f由如下规则定义：
如果n<3，那么f（n）=n，如果n>=3，那么f（n）=f（n-1）+f（n-2）+f（n-3）
其实就是初始项是0 1 2 ，然后后面每一项就是前3项的和
|#

; 递归
(define (f n)
    (cond ((< n 3) n)
          ((>= n 3) (+ (f (- n 1)) (f (- n 2)) (f (- n 3))))
    )
)

(f 9)
(newline)


; 迭代写法
(define (fi a b c count)
    (if (= count 0)
        a
        (fi b c (+ a b c) (- count 1))
    )
)
(define (fn n)
    (fi 0 1 2 n)
)

(fn 9)

