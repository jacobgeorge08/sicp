#lang sicp

(define x (list (list 1 2) (list 3 4)))
(define y (list x x))

(define (fringe tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree)) (fringe (cdr tree))))))

(fringe x)
(fringe y)
