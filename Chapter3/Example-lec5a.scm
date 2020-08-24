#lang sicp

#|
以阶乘为例，首先是函数式编程
|#

(define (fact n)
    (define (iter i m)
        (cond ((> i n) m)
              (else
                (iter (inc i) (* i m))
              )
        )
    )
    (iter 1 1)
)

(fact 4)

(define (fact2 n)
    (if (= n 1)
        1
        (* n (fact2 (- n 1)))
    )
)

(fact2 5)
#|
赋值的状态编程
|#

(define (fact1 n)
    (let ((i 1) (m 1))
        (define (loop)
            (cond ((> i n) m)
                  (else
                    (set! m (* i m))
                    (set! i (inc i))
                    (loop)
                  )
            )
        )
        (loop)
    )
)
(fact1 6)
(fact1 6)

#|
影片中的例子，一个计数器.

外层的lambda可以接收计时器的初始值n作为参数，但是对n的具体操作是内层的lambda，
这样就可以将外层lambda的n作为一个局部变量，从而保持它的状态。
并且通过define不同的实例（c1 c2）指向make-counter，从而达到各自维护各自内部变量的目的
|#

(define make-counter
    (lambda (n)
        (lambda () (set! n (inc n)) n)
    )
)
(define c1 (make-counter 0))
(c1)
(c1)
(define c2 (make-counter 10))
(c2)
(c2)

#|
还有个等价的写法，我们认为但是这种写法的抽象层次和第一种还是有差别的，影片中并没有阐述。

第一种更加抽象，应该更能够表达”环境“，”框架“这种思想。
|#

(define (make-counter1 n)
    (lambda () (set! n (inc n)) n)
)
(define cc1 (make-counter1 0))
(cc1)
(cc1)
(define cc2 (make-counter1 10))
(cc2)
(cc2)
