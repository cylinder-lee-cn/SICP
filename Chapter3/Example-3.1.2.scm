#lang sicp

#|
设计一个过程rand，每次调用的时候就会返回一个随机整数

可以将rand实现成为一个带有局部状态变量x的过程，可将这个变量初始化为某个固定值random-init。
对于rand的每次调用都算出当前x值的rand-update值，存入x并返回x

也就是利用random-init为随机数的种子，每次利用新生成的x作为种子来产生下一个随机数
|#
(define rand
    (let ((x random-init))
        (lambda ()
            (set! x (rand-update x))
            x
        )
    )
)

#|
蒙特卡罗模拟，随机选取两个整数之间没有公共因子（最大公因数是1，(gcd a b)是1）的概率是6/pi^2
|#

(define (estimate-pi trails)
    (sqrt (/ 6 (monte-carlo trails cesaro-test)))
)
(define (cesaro-test)
    (= (gcd (rand) (rand)) 1)
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

#|
如果没有rand，直接使用rand-update完成同一个计算，将不得不采用如下的做法

这种方法就无法将检测方法抽象出来成为一个独立的过程，从而无法将蒙特卡罗方法自身抽象出来
|#

(define (estimate-pi trails)
    (sqrt (/ 6 (random-gcd-test trails random-init)))
)

(define (random-gcd-test trails initial-x)
    (define (iter trails-remaining trails-passed x)
        (let ((x1 (rand-update x)))
            (let ((x2 (rand-update x1)))
                (cond ((= trails-remaining 0)
                        (/ trails-passed trails)
                      )
                      ((= (gcd x1 x2) 1)
                        (iter (- trails-remaining 1) (+ trails-passed 1) x2)
                      )
                      (else
                        (iter (- trails-remaining 1) trails-passed x2)
                      )
                )
            )
        )
    )
    (iter trails 0 initial-x)
)