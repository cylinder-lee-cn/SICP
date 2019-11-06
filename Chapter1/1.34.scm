#lang sicp
(define (f g) (g 2))
(define (square x) (* x x))
(f square)
(f (lambda (x) (* x (+ x 1))))

#|
如果调用（f f）会发生什么？

会报错：
application: not a procedure;
 expected a procedure that can be applied to arguments
  given: 2
  arguments...:

调用(f f) 那函数f就作为参数传到(g 2)中
就相当于(f 2)，调用(f 2)就相等于把2 作为参数传到(g 2)中
就成了(2 2),2不是已经定义函数或者是操作，所以报错
|#