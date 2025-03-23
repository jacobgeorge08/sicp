#lang sicp

(define a (list 1 3 (list 5 7) 9))
(define b (list (list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

(define (a7 a)
  (car (cdr (car (cdr (cdr a))))))
(a7 a)

(define (b7 b)
  (car (car b)))
(b7 b)

(define (c7 c)
  (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c)))))))))))))
(c7 c)


