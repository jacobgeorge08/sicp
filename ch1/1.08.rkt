#lang sicp

(define (cube x)
  (* x x x))

(define (improve guess x)
  (/ (+ (/ x ( * guess guess)) (* 2 guess)) 3))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (cbrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (cbrt-iter (improve guess x) x)))


(define (cbrt x)
  (cbrt-iter 1.0 x))

(cbrt 64)
