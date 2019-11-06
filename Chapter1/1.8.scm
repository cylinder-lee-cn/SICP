#lang sicp
; 求立方根的牛顿法基于如下事实，如果y是x的立方根的一个近似值，那么下式将给出一个更好地近似值
; (x/y*y+2y)/3

(define (improve y x)
    (/ (+ (/ x (* y y)) (* y 2)) 3)
)

(define (good-enough2? oldguess newguess)
    (> 0.001 (/ (abs (- newguess oldguess)) oldguess)
    )
)

(define (cube-root guess x)
    (if (good-enough2? guess (improve guess x))
        (improve guess x)
        (cube-root (improve guess x) x))
)



(display (cube-root 1.0 8))

; (display (improve 1.0 8))


; 此题主要是修改improve这个函数