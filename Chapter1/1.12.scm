#lang sicp
#|
这道练习的翻译有误，
原文是『...Write a procedure that computes elements of Pascal’s triangle by means of a recursive process.』，
译文只翻译了『。。。它采用递归计算过程计算出帕斯卡三角形。』，这里应该是『帕斯卡三角形的各个元素』才对。
使用示例图可以更直观地看出帕斯卡三角形的各个元素之间的关系：

row:
0        1
1       1 1
2      1 2 1
3     1 3 3 1
4    1 4 6 4 1
5   . . . . . .
col: 0 1 2 3 4

当 row=col时，也就是每行的头尾，元素都是1，col=0时 元素=1，row=0时元素=1

f(row,col)=f(row-1,col-1)+f(row-1,col)

行列都从 1 开始定义也没问题
|#

(define (f row col)
    (cond ((= row col) 1)
          ((or (= row 0) (= col 0)) 1)
          (else (+ (f (- row 1) (- col 1)) (f (- row 1) col)))
    )
)

(f 4 2)