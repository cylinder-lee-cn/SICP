#lang sicp

(car ''abracddabra)

#|
这个写法等同于
 (car (quote (quote abracddabra)))
=(car '(quote abracddabra))
=quote
|#
