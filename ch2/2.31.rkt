#lang sicp

(define (square x)
  (* x x))

(define (square-tree tree) (tree-map square tree))
(define (squaretree tree) (treemap square tree))

(define (treemap f tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (f tree))
        (else (cons (treemap f (car tree)) (treemap f (cdr tree))))))

(define (tree-map f tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (tree-map f subtree)
             (f subtree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(squaretree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
