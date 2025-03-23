#lang sicp

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list (list 3 4) (list 5 6 7))))

(define (deep-reverse items)
  (cond ((null? items) nil)
        ((not (pair? items)) items)
        (else (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))

(define (dreverse items)
  (define (dreverse-iter remaining result)
    (cond ((null? remaining) result)
          ((not (pair? remaining)) remaining)
          (else (dreverse-iter (cdr remaining)
                               (cons (dreverse (car remaining)) result)))))
  (dreverse-iter items nil))

x
(deep-reverse x)
(dreverse x)
y
(deep-reverse y)
(dreverse y)
