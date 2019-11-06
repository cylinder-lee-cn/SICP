(define (expmod base exp m)
    (cond ((= exp 0) 1)
          ((even? exp)
            (remainder (* (expmod base (/ exp 2) m)
                          (expmod base (/ exp 2) m)
            ) m)
          )
          (else 
            (remainder (* base (expmod base (- exp 1) m)) m)
          )
    )
)

#|
这样写效率很低，是因为当exp为偶数时， (expmod base (/ exp 2) m) 会被执行2次
将O(log n) 变成了 O(n)
|#