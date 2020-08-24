#lang sicp

#|
获取某个范围内的随机数，用实数比用整数的效果好，分布的更加随机且均匀，
尤其是在选取的上下限比较小的情况下

p是一个过程，能够接收两个参数（x y），并且验证x y是否在圆内，这里把判断规则给写死了。
同时，x y取值的范围要来自外部参数，所以将p再次包装成了一个lambda函数，来接收 x1 x2 y1 y2
4个参数。

如果要活用x1 x2 y1 y2，那就需要重新构造p?，将p?放到monte-carlo调用时用lambda来重新定义，
从而使用x1 x2 y1 y2
|#


(define (square x) (* x x))

(define (estimate-integral p x1 x2 y1 y2 trails)
    (* 4.0 
        (monte-carlo trails (lambda () 
                                (p (random-in-range x1 x2) 
                                   (random-in-range y1 y2)))
        )
    )
)

(define (p? x y)
    (< (+ (square x) (square y)) 1.0)
)

(define (random-in-range low high)
    (let ((range (- high low)))
        (+ low (random range))
    )
)

(define (monte-carlo trails experiment)
    (define (iter trails-remaining trails-passed)
        (cond ((= trails-remaining 0)
                (/ trails-passed trails)
              )
              ((experiment)
                (iter (- trails-remaining 1) (+ trails-passed 1))
              )
              (else
                (iter (- trails-remaining 1) trails-passed)
              )
        )
    )
    (iter trails 0)
)


(estimate-integral p? 0.0 1.0 0.0 1.0 100000)