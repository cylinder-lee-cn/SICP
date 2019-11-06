#lang sicp
#|
换零钱方式：
有1 5 10 25 50 美分的硬币，将1美元换成硬币有多少种换法

* 将总数是a的现金换成n种硬币的不同方式的数目等于
1 将现金数a换成除一种硬币以外所有其他硬币的不同组合的数量
2 将现金数a-d换成所有硬币的不同组合的数量，d是1中选择的硬币的币值
3 将1和2的结果相加，将所有的硬币种类都用一遍

比如：
100(1 5 10 25)所有硬币组合 + (100-50)(1 5 10 25 50)硬币组合 ，选50硬币
100(1 5 10 50)所有硬币组合 + (100-25)(1 5 10 25 50)硬币组合 ，选25硬币
100(1 5 25 50)所有硬币组合 + (100-10)(1 5 10 25 50)硬币组合 ，选10硬币
100(1 10 25 50)所有硬币组合 + (100-5)(1 5 10 25 50)硬币组合 ，选5硬币
100(5 10 25 50)所有硬币组合 + (100-1)(1 5 10 25 50)硬币组合 ，选1硬币

如果 a就是0，那么只有1种换法
如果 a小于0，那么就是0种换法
如果 n是0，那么就是0种换法
|#

(define (count-change amount)
    (cc amount 5))

(define (cc amount kinds-of-coins)
    (cond ((= amount 0) 1)
    ((or (< amount 0) (= kinds-of-coins 0)) 0)
    (else (+ (cc amount 
                 (- kinds-of-coins 1))
             (cc (- amount
                    (first-denomination kinds-of-coins))
                  kinds-of-coins)))))    

(define (first-denomination kinds-of-coins)
    (cond ((= kinds-of-coins 1) 1)
          ((= kinds-of-coins 2) 5)
          ((= kinds-of-coins 3) 10)
          ((= kinds-of-coins 4) 25)
          ((= kinds-of-coins 5) 50))
)

(count-change 11)


