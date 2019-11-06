#lang sicp
#|
俄罗斯农民的乘法：
* 把每一个数字分别写在列头。
* 将头一列的数字加倍，将第二列的数字减半。
     如果在第二列的数字是奇数，将它除以二并把余数去掉。
* 如果第二列的数字是偶数，将其所在行删除。
* 继续加倍、减半和删除直到第二列的数字为1。
* 将第一列中剩余的数字相加。于是就得出了根据原始数字计算出的结果。

让我们以计算57乘以86为例。
    把每一个数字分别写在列头。
        57     86
    将头一 列的数字加倍，将第二 列的数字减半。
        57     86
        114     43
    如果第二 列的数字是偶数，将其所在行删除。
        57     86   <- del
        114     43
    继续加倍、减半和删除直到第二 列的数字为1。
        57     86   <- del
        114     43
        228     21
        456     10  <- del
        912      5
        1824     2  <- del
        3648     1
    将第一 列中剩余的数字相加。于是就得出了根据原始数字计算出的结果。
        57     86   <- del
        114     43
        228     21
        456     10  <- del
        912      5
        1824     2  <- del
    +  3648     1
            4902
|#

(define (double x) (+ x x))
(define (havle x) (quotient x 2))

(define (multi-iter a b result)
    (cond ((= b 0) 0)
          ((= b 1) (+ result a))
          ((even? b) (multi-iter (double a) (havle b) result))
          ((odd? b) (multi-iter (double a) (havle b) (+ result a)))
    )
)

(define (multi a b) (multi-iter a b 0))


(double 9)
(newline)
(havle 1)
(newline)
(multi 7 8)

