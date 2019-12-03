#lang sicp

(define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (inc low) high))
    )
)

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence) (accumulate op initial (cdr sequence)))
    )
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (filter predicate sequence)
    (cond ((null? sequence) nil)
          ((predicate (car sequence)) 
            (cons (car sequence) (filter predicate (cdr sequence)))
          )
          (else 
            (filter predicate (cdr sequence))
          )
    )    
)

(define (queens board-size)
  (define (queen-cols k) 
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board nil)


(define (adjoin-position a b c)
    (cons a c)
)

(define (safe? k queen-position)
    (define (check-next? queen-pos positions row-move)
        (if (null? positions)
            #t
            (let ((queen-exist (car positions)))
                (if (or (= queen-exist queen-pos)
                        (= (+ queen-exist row-move) queen-pos)
                        (= (- queen-exist row-move) queen-pos)
                    )
                    #f
                    (check-next? queen-pos (cdr positions) (inc row-move))
                )
            )
        )
    )
    (check-next? (car queen-position) (cdr queen-position) 1)
)

(queens 6)

#|
此题解法：
* 首先利用了两个map，巧妙的穷举出了所有queen位置的组合。
* 利用queen-cols过程的递归，每次递归都会执行2个map。
* 以棋盘3x3为例，将(queens 3)展开：
    (queen 3)
    -> (queen-cols 3)
        ->(queen-cols 2)
            -> (queen-cols 1)
                -> (queen-cols 0)

* (queen-cols 0) 结果是(list nil) 是一个空的list
    * safe?检查(list nil)，没有格子，没有queen，应该返回#t

* (queen-cols 1)就是
    (flatmap (λ (y))
        (map (λ (x) (cons x y)) (list 1 2 3))
        (list nil)
    )
    结果是：
    ((1)(2)(3))
    * safe?检查((1)(2)(3))，棋盘第1行，queen放在1-2-3任何一个格子里都OK，应该返回#t

* (queen-cols 2)就是
    (flatmap (λ (y))
        (map (λ (x) (cons x y)) (list 1 2 3))
        ((1)(2)(3))
    )
    结果是：
    ((1 1)(2 1)(3 1)(1 2)(2 2)(3 2)(1 3)(2 3)(3 3))
    * safe?检查((1 1)(2 1)(3 1)(1 2)(2 2)(3 2)(1 3)(2 3)(3 3))，
    棋盘第2行，(1 1) (2 1) (1 2) (2 2) (3 2) (2 3) (3 3)，都#f，检查同列和对角线
    剩下((3 1) (1 3))

* (queen-cols 3)就是
    (flatmap (λ (y))
        (map (λ (x) (cons x y)) (list 1 2 3))
        ((1 1)(2 1)(3 1)(1 2)(2 2)(3 2)(1 3)(2 3)(3 3))
    )
    结果是：
    ((1 1 1)(1 2 1)(1 3 1)(1 1 2)(1 2 2)(1 3 2)(1 1 3)(1 2 3)(1 3 3)
     (2 1 1)(2 2 1)(2 3 1)(2 1 2)(2 2 2)(2 3 2)(2 1 3)(2 2 3)(2 3 3)
     (3 1 1)(3 2 1)(3 3 1)(3 1 2)(3 2 2)(3 3 2)(3 1 3)(3 2 3)(3 3 3)
    )
    * safe?检查((1 3 1)(2 3 1)(3 3 1)(1 1 3)(2 1 3)(3 1 3))，
    棋盘第3行，都#f，检查同列和对角线，返回()

* 在每次queen-cols递归的同时，还会调用filter，利用其中的safe?来检查每次增加queen的位置是否
  能够满足解的条件（横向没有，纵向没有，对角线没有）
* 其中横向不用检测。比如(1 3 2)这个list的元素下标表示不同的行，元素的内容代表列。
  (1 3 2)表示第一行queen在第一列，第二行queen在第3列，第三行queen在第二列
* 由于每次queen-cols递归的时候，会在每个组合的左侧新增queen的位置组合。
  所以safe?只需要car每个组合，将其与cdr内所有的元素进行比较。
|#

