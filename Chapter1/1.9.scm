#lang sicp
; 定义了一种两个正整数相加的方法，他们都基于inc（参数增加1）和dec（参数减少1）
; 用plus 代替 +

(define (plus a b)
    (if (= a 0)
        b
        (inc (plus (dec a) b)))
)

; 如果是调用 （plus 4 5）
(plus 4 5)
(inc (plus 3 5))
(inc (inc (plus 2 5)))
(inc (inc (inc (plus 1 5))))
(inc (inc (inc (inc (plus 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
; 这是一个展开和收缩的过程，是线性递归

(define (plus a b)
    (if (= a 0)
        b
        (plus (dec a) (inc b))
    )
)
; 如果是调用（plus 4 5）
(plus 4 5)
(plus 3 6)
(plus 2 7)
(plus 1 8)
(plus 0 9)
9
; 没有收缩和展开，只有常量存储大小，计算次数和a成正比，是线性迭代
