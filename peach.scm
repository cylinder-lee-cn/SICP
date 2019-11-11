#lang sicp
#|
果园里有堆苹果，N（1＜N＜9）只熊来分。第一只熊把这堆苹果平均分为N份，多了k个，它把多的k个扔了，拿走了一份。
第二只熊把剩下的苹果又平均分成N份，又多了k个，它同样把多的k个扔了，拿走了一份，
第三、第四直到第N只熊都是这么做的，问果园里原来最少有多少个苹果？

到最后一只熊的时候，最少要分到一个，所以当时的苹果应该是result=m*n+k，
这个结果应该可以被(n-1)整除，因为是上一次平均分成n份以后剩下的n-1份，
所以倒数第二只面临的的苹果数是result/(n-1)*n+k，依次类推。
如果result不能被(n-1)整除，那么最后一只熊分到的就不是1个苹果，就增加1，再继续整个分配的测试。

result是最后苹果的总数
m是最后熊分到的苹果
n是熊的数量
k是余数
i初始和n相同，每个熊都要处理一次苹果
|#
(define (peach result m n k i)
  (cond ((= i n) (peach (+ (* m n) k) m n k (dec i)))
        ((= i 0) result)
        ((= 0 (remainder result (- n 1))) 
          (peach (+ (* (/ result (- n 1)) n) k) m n k (dec i))
        )
        (else 
          (peach result (inc m) n k n)
        )
  )
)

(peach 0 1 5 1 5) 