#lang sicp
#|
第一个pair 中间数是X，误差是Px
X*(1-Px) X(1+Px)

第二个pair 中间数是Y，误差是Py
Y*(1-Py) Y(1+Py)

两个pair相乘就是

X*(1-Px)*Y*(1-Py)
=X*Y*(1-Px)*(1-Py)
=X*Y*(1-Px-Py+PxPy)

X*(1+Px)*Y*(1+Py)
=X*Y*(1+Px)*(1+Py)
=X*Y*(1+Px+Py+PxPy)

如果Px和Py都是很小的百分数，那么Px*Py就是更小的百分数，比如0.03和0.05，0.03*0.05=0.0015
假如将其忽略，那么
X*Y*(1-Px-Py+PxPy)
=X*Y*(1-Px-Py)
=X*Y*(1-(Px+Py))

X*Y*(1+Px+Py+PxPy)
=X*Y*(1+Px+Py)
=X*Y*(1+Px+Py)

也就是两个pair乘积的误差可以认为是每个pair误差的和
|#


