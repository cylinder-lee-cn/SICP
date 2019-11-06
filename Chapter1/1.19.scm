#lang sicp
#|
This exercise is fairly easy if we observe that, if we group a and b into a column vector, T is a linear transformation expressed by

1	1
1	0
and Tpq is, similarly,

p+q	q
q	p
Given this definition of Tpq, Tp'q' can be easily computed as the square of Tpq. p' and q' are, respectively

p' = p^2 + q^2
q' = 2pq + q^2
The same results can be computed without resorting to linear algebra. Just define a' and b' by applying the transformation once

a' = qb +qa + pa = (p + q)a + bq
b' = qa + pb
Then, let a" and b" be the results of applying the transformation to a' and b' and show how those values can be computed directly from a and b

a" = qb' +qa' + pa' = (p + q)a' + b'q = ... = (p^2 + 2pq + 2q^2)a + (2pq + q^2)b
b" = qa' + pb' = ... = (2pq + q^2)a + (p^2 + q^2)b
Now, it is plain to see that p' and q' are the same as the ones shown above.
|#

(define (fib n)
    (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fib-iter 
                                    a 
                                    b 
                                    (+ (* p p) (* q q)) 
                                    (+ (* 2 p q) (* q q)) 
                                    (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p)) 
                          (+ (* b p) (* a q) ) 
                          p 
                          q 
                          (- count 1)))
    )
)

(fib 10)



