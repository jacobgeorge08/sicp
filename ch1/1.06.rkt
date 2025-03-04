#lang sicp

; If is a special form and does not evaluate like other functions which do apllicative eval 
; if does normal order evalution
; The new if function defined here calls the sqrt-iter-new function irrespective of what happes (applicative eval) and 
; we get infinite recursion

(define (square x)
  (* x x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 100)

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x)))

(define (sqrtN x)
  (sqrt-iter-new 1.0 x))

(sqrtN 100)

