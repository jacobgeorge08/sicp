#lang sicp

(define (square-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree)) (square-tree (cdr tree))))))

(define (square x)
  (* x x))

(define (map-tree tree)
  (map (lambda (subtree)
         (if (pair? subtree)
             (map-tree subtree)
             (square subtree)))
       tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(map-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
