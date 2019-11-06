#lang sicp
; Alyssa P. Hacker看不出为什么需要将if提供为一种特殊形式，她问：为什么我不能直接
; 通过cond将它定义为一个常规过程呢？ Eva Lu Ator断言确实可以这样做，并定义了if的新版

(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause))
)

; Eva演示的程序
(display (new-if (> 2 3) 0 5))

(display (new-if (= 1 1) 0 5))


; Alyssa用new-if 重写了求平方根的程序
(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)
    )
)

; 会发生什么事情？请给出解释。

; 会陷入死循环，这个和1.5的练习很类似，if必须是个特殊形式，如果是常规过程，由于编译器是应用序，
; 所以会先求参数的值，那么计算出(improve guess x)的值，然后继续调用sqrt-iter，不管new-if的
; 判断结果如何都会递归的触发sqrt-iter，陷入死循环。应该用标准if这样才会真正的决定是否会递归调用