#lang racket
(#%require sicp-pict)

#|
只能在racket的环境下运行并显示结果
|#

(paint (segments->painter
(list
				(make-segment (make-vect 0 .5) (make-vect .1875 .3125))
				(make-segment (make-vect .1875 .3125) (make-vect .3125 .5625))
				(make-segment (make-vect .3125 .5625) (make-vect .375 .5))
				(make-segment (make-vect .375 .5) (make-vect .25 0))
				(make-segment (make-vect .375 0) (make-vect .5 .3125))
				(make-segment (make-vect .5 .3125) (make-vect .625 0))
				(make-segment (make-vect .75 0) (make-vect .625 .375))
				(make-segment (make-vect .625 .375) (make-vect 1 .1875))
				(make-segment (make-vect 1 .375) (make-vect .75 .625))
				(make-segment (make-vect .75 .625) (make-vect .5625 .625))
				(make-segment (make-vect .5625 .625) (make-vect .6875 .8125))
				(make-segment (make-vect .6875 .8125) (make-vect .5625 1))
				(make-segment (make-vect .4375 1) (make-vect .3125 .8125))
				(make-segment (make-vect .3125 .8125) (make-vect .4375 .625))
				(make-segment (make-vect .4375 .625) (make-vect .25 .625))
				(make-segment (make-vect .25 .625) (make-vect .125 .5))
				(make-segment (make-vect .125 .5) (make-vect 0 .625))
                (make-segment (make-vect 0.35 0.85) (make-vect 0.45 0.85))
                (make-segment (make-vect 0.55 0.85) (make-vect 0.65 0.85))
                (make-segment (make-vect 0.45 0.75) (make-vect 0.55 0.75))

