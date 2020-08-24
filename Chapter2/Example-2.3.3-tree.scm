#lang sicp

(define (entry tree)
    (car tree)
)

(define (left-branch tree)
    (cadr tree)
)

(define (right-branch tree)
    (caddr tree)
)

(define (make-tree entry left right)
    (list entry left right)
)

#|
在二叉树中查找一个元素是否存在，这个二叉树的左边的元素一定小于顶的entry，右边的元素一定大于顶的entry
属于平衡二叉树，查找一个元素是否在树中，那就先和顶部比较，如果比entry大那么就搜索右侧子树，否则就寻找左侧子树
|#
(define (element-of-tree? x tree)
    (cond ((null? tree) false)
          ((= x (entry tree)) true)
          ((< x (entry tree)) (element-of-tree? x (left-branch tree)))
          ((> x (entry tree)) (element-of-tree? x (right-branch tree)))
    )
)

(define atree `(7 (3 (1 () () ) (5 () () )) (9 () (11 () () ))))

; (make-tree 7 (make-tree 3 1 5) (make-tree 9 nil 11))

(element-of-tree? 0 atree)

(define (adjoin-tree x tree)
    (cond ((null? tree) (make-tree x `() `()))
          ((= x (entry tree)) tree)
          ((< x (entry tree)) (make-tree (entry tree) (adjoin-tree x (left-branch tree)) (right-branch tree)))
          ((> x (entry tree)) (make-tree (entry tree) (left-branch tree) (adjoin-tree x (right-branch tree))))
    )
)

(adjoin-tree 10 atree)
