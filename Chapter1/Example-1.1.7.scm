#lang sicp
; 1.1.7 实例：用牛顿法求一个数的平方根

; 计算两个数的平均数
(define (average  x y)
    (/ (+ x y) 2))

; 改进guess，就是计算guess和guess/x的平均值
(define (improve guess x)
    (average guess (/ x guess))
)

; 检测guess是否够好，就是guess的平方-x的绝对值是否小于0.001，就是精确度的问题
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001)
)

; 计算某个数的平方
(define (square y) (* y y))

; 求x平方根的函数，采用了递归。
; guess从1.0开始，对guess和x进行比较判断绝对值是 abs(guess*guess-x) < 0.001
; 如果不是，那么改进guess，就是计算 guess和guess/x的平均值作为guess
; 继续调用计算平方根的函数，

(define (sqrt-iter guess x)
    (if (good-enough? guess x) 
        guess
        (sqrt-iter (improve guess x) x))
)

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause))
)

(define (sqrt-iter2 guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter2 (improve guess x) x)
    )
)

(sqrt-iter2 1.0 2)

