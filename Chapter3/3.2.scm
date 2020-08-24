#lang sicp

(define (make-monitored foo)
    (let ((count 0))
        (define (mf m)
            (cond ((eq? m `how-many-calls?) count)
                  ((eq? m `reset-count) 
                    (begin
                        (set! count 0)
                        count
                    )
                  )
                  (else 
                    (begin
                        (set! count (inc count))
                        (foo m)
                    )
                  )
            )
        )
        mf
    )
)

(define s (make-monitored sqrt))

(s 9)
(s 100)
(s 4)
(s `how-many-calls?)
(s `reset-count)
(s 8)
(s 7)
(s `how-many-calls?)


#|
如果是某个过程内要用到的局部变量，需要定义在这个过程的外面

(let ((variable value))
    (在这里定义过程，这个过程将能重复的使用外面定义的局部变量，不会每次都被初始化掉))

|#
