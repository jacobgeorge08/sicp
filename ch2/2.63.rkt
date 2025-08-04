#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1
                     (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list
                             (right-branch tree)
                             result-list)))))
  (copy-to-list tree '()))

(define (tree-list2 tree)
  (define (copy tree result)
    (if (null? tree)
        result
        (copy (left-branch tree)
              (cons (entry tree)
                    (copy
                     (right-branch tree)
                     result)))))
  (copy tree '()))

(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))

(tree->list-1 t1)
(tree->list-1 t2)
(tree->list-1 t3)

(tree->list-2 t1)
(tree->list-2 t2)
(tree->list-2 t3)

;; Okay so both procedures take a tree and convert them to a list
;; Not only do we get a list but also an in order list for both
;; procedures

;; (tree->list-1) builds a recursive procedure while
;; (tree->list-2) builds a iterative procedure with the accumulator result

;; They do not have the same order of growth and (tree->list-1) builds
;; a stack of recursive procedures that unwinds slower
;;
;; While (tree->list-2) is much more efficient since the current state
;; of the procedure is always available via the result accumulator
;;


