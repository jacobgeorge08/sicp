#lang sicp
;Using reasoning analogous to Alyssa’s, de- scribe how the diﬀerence of two intervals may be com- puted. Deﬁne a corresponding subtraction procedure, called sub-interval.

(define (make-interval a b) (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

