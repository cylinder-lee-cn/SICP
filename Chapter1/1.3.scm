#lang sicp
;定义一个过程，它以3个数为参数，返回其中两个较大的两个数之和。
;此题解法，求出最小的那个，然后用3个数的总和减去最小的即可

(define (sumbig a b c) 
    (- (+ a b c) (min a b c))
)

;(display(m (m 9 7) 3))
(display (sumbig 9 7 3))
