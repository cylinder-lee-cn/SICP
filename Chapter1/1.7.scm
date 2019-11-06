#lang sicp
; 对于确定很小的数的平方根而言，在计算平方根中使用的检测 good-enough？是很不好的。
; 还有在现实计算中，总是以一定的有限的精度进行，也不适合非常大的数计算。
; 实现good-enough？的另一种策略就是比较guess上一次和这一次的变化情况，
; 当改变值相对于猜测值的比率很小时就结束。

; 计算两个数的平均数
(define (average  x y)
    (/ (+ x y) 2))

; 改进guess，就是计算guess和guess/x的平均值
(define (improve guess x)
    (average guess (/ x guess))
)

; 检测guess是否够好，就是guess的平方-x的绝对值是否小于0.001，就是精确度的问题
(define (good-enough? guess x)
    (< 0.001 (abs (- (square guess) x)))
)

; 新的good-enough策略
(define (good-enough2? oldguess newguess)
    (> 0.01 (/ (abs (- newguess oldguess)) oldguess)
    )
)

; 计算某个数的平方
(define (square y) (* y y))

; 求x平方根的函数，采用了递归。
; guess从1.0开始，对guess和x进行比较判断绝对值是 abs(guess*guess-x) < 0.001
; 如果不是，那么改进guess，就是计算 guess和guess/x的平均值作为guess
; 继续调用计算平方根的函数，

(define (sqrt-iter guess x)
    (if (good-enough2? guess (improve guess x))
        (improve guess x)
        (sqrt-iter (improve guess x) x))
)



(display (sqrt-iter 1.0 0.000009))

