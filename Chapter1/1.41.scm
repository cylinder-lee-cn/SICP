#lang sicp

(define (double f)
    (lambda (x)
        (f (f x))
    )
)
((double inc) 1)


(((double (double double)) inc) 5)

#|
(double f) -> (f (f x))

(((double double) f) x)
=((d (d f)) x)
=((d f) ((d f) x))
=(f(f((d f) x)))
=(f(f(f(f x))))

(((double (double double)) f) x)
(((d d) ((d d) f)) x)
(((d d) (d (d f))) x)
(((d (d (d (d f))))) x)

f-> 2x2x2x2=16(times)
|#
