;Deﬁne a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.
#lang sicp

(define (square x)
  (* x x))

(define (sum-of-sq x y)
  (+ (square x) (square y)))

(define (three-sq x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-sq y z))
        ((and (<= y x) (<= y z)) (sum-of-sq x z))
        (else (sum-of-sq x y))))

(three-sq 3 4 5)

