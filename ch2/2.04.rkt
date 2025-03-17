#lang scheme

(define (cons x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (q r) q)))

(define (cdr2 z)
  (z (lambda (q r)r)))

(cdr2 (cons 1 2))
(car2 (cons 1 2))

