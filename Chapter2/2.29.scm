#lang sicp

(define (make-mobile left right)
    (list left right)
)

(define (make-branch blength structure)
    (list blength structure)
)

(define (left-branch mobile)
    (car mobile)
)

(define (right-branch mobile)
    ; (cadr mobile) ;用第一种构造方法
    (cdr mobile)    ;用第二种构造方法
)

(define (branch-length branch)
    (car branch)
)
(define (branch-structure branch)
    ; (cadr branch) ;用第一种构造方法
    (cdr branch)    ;用第二种构造方法
)



(define mrrl (make-branch 5 4))
(define mrrr (make-branch 8 2))
(define mrr (make-mobile mrrl mrrr))

(define ml (make-branch 4 9))
(define mr (make-branch 6 mrr))

(define root (make-mobile ml mr))

root
; (branch-length mr)


;总重量是左分支的重量+右分支的重量
(define (mobile-weight mobile)
    (+ (branch-weight (left-branch mobile))
       (branch-weight (right-branch mobile))
    )
)

;如果右边是structure那么，重量就是用total-weight计算，否则就是重量自身
(define (branch-weight branch)
    (if (is_structure? branch)
        (mobile-weight (branch-structure branch))
        (branch-structure branch)
    )
)
;判断右边是否是重量or结构
(define (is_structure? branch)
    (pair? (branch-structure branch))
)

; (total-weight mrr)

#|
活动体是否平衡，判断依据就是左右两个branch的：
左length * 左weight == 右length * 右weight
|#
(define (branch-torque branch)
    (* (branch-length branch)
        (branch-weight branch)
    )
)

#|
一个mobile是否平衡，需要左右两个branch的torque相同
而且每个branch也平衡
|#

(define (mobile-balance? mobile)
    (and
        (= (branch-torque (left-branch mobile)) (branch-torque (right-branch mobile)))
        (branch-balance? (left-branch mobile))
        (branch-balance? (right-branch mobile))
    )
)

#|
branch平衡，如果branch里不包含mobile，那么就平衡，如果包含mobile，还要检测mobile是否平衡
|#

(define (branch-balance? branch)
    (if (is_structure? branch)
        (mobile-balance? (branch-structure branch))
        true
    )
)

; (branch-torque ml)
; (branch-torque mr)

; (mobile-balance? mrr)

#|
修改活动体的定义，只用修改两个地方：就是取右branch，List用cadr取，cons用cdr取
|#

(define (make-mobile1 left right)
    (cons left right)
)

(define (make-branch1 mlength structure)
    (cons mlength structure)
)

(define mrrl1 (make-branch1 5 4))
(define mrrr1 (make-branch1 10 2))
(define mrr1 (make-mobile1 mrrl1 mrrr1))

(define ml1 (make-branch1 4 9))
(define mr1 (make-branch1 6 mrr1))

(define root1 (make-mobile1 ml1 mr1))

; (mobile-weight root1)
; (mobile-balance? mrr1)
; (mobile-balance? root1)

(define (balance? mobile)
    (let ((lb (left-branch mobile))
          (rb (right-branch mobile))
         )
         (cond ((not (= (branch-torque lb) (branch-torque rb))) false)
               ((is_structure? lb) (balance? (branch-structure lb)))
               ((is_structure? rb) (balance? (branch-structure rb)))
               (else true)
         )
    )
)

(balance? root1)