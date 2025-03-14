#lang sicp

(define (average x y)
   (/ (+ x y) 2))

(define (square x) (* x x))

(define (iterative-improve good-enough? improve)
  (define (iter-guess guess)
    (if (good-enough? guess)
      guess
      (iter-guess (improve guess))))
  iter-guess)

(define (sqrt x)
   ((iterative-improve (lambda (guess)
                         (< (abs (- (square guess) x))
                            0.001))
                       (lambda (guess)
                         (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
   ((iterative-improve (lambda (guess)
                         (< (abs (- (f guess) guess))
                            0.00001))
                       (lambda (guess)
                         (f guess)))
    first-guess))

(sqrt 25)

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)
