#lang sicp
#|
区间宽度的定义是：上界和下界之差的一半

X的宽度是：(Xu-Xl)/2
Y的宽度是：(Yu-Yl)/2

X宽度+Y宽度：
((Xu-Xl)+(Yu-Yl))/2=(Xu-Xl+Yu-Yl)

按照add-internal，X+Y是：
(Xl+Yl),(Xu+Yu)
那X+Y的宽度就是：
((Xu+Yu)-(Xl+Yl))/2
打开括号后整理
(Xu+Yu-Xl-Yl)/2 与 X宽度+Y宽度一致
|#
