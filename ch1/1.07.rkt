#lang sicp

; For small numbers example 0.00009, our tolerance for good enough was larger than the sqroot resulting in 
; inaccurate guesses
; For larger numbers, even if the guess is off by 1, the difference between the guess and the number would be massive 
; and it would take forever(maybe never) to reach the guess
;
(define (square x)
  (* x x))

(define (good-enough? previous-guess guess)
  (< (abs (/ (- guess previous-guess) guess)) 0.00000000001))

(define (sqrt-iter guess x)
  (if (good-enough? guess (improve guess x))
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))


(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 0.0009)
(sqrt 1000000000000000)


