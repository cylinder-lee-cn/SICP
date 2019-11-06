lang sicp
#|
模拟应用序和正则序分别去解释 gcd 函数，并统计 remainder 的调用次数
|#
(define (gcd a b)
    (if (= b 0)
        a
        (gcd b (remainder a b))
    )
)

(define (isgcd? i n)
    (if (and (= 1 (gcd i n)) (< i n))
        i
        1
    )
)
; (provide (all-defined-out))


#|
trace的结果如下：

> (gcd 206 40)
|(gcd 206 40)   -> (gcd 40 (remainder 206 40))
|(gcd 40 6)     -> (gcd 6 (remainder 40 6))
|(gcd 6 4)      -> (gcd 4 (remainder 6 4))
|(gcd 4 2)      -> (gcd 2 (remainder 2 2))
|(gcd 2 0)      -> b==0 --- 这个就不执行remainder了
|2
2

如果是应用序，remainder应该执行4次

------------------------------------
|(gcd 206 40)   -> (gcd 40 (r 206 40))
|(gcd 40 6)     -> (gcd (r 206 40) (r 40 (r 206 40)))
|(gcd 6 4)      -> (gcd (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40))))
|(gcd 4 2)      -> (gcd (r (r 206 40) (r 40 (r 206 40))) (r (r 40 (r 206 40)) (r (r 206 40) (r 40 (r 206 40)))))
|(gcd 2 0)      -> ...
如果是正则序 
|#