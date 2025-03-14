#lang sicp

(define (average x y z)
  (/ (+ x y z) 3))

(define (square x)
  (* x x))

(define dx 0.0001)

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
    (if (= n 1) 
      f
      (compose (repeated f (- n 1)) f))) 

(define (smoothed f)
  (lambda (x)
    (average (f x) (f (- x dx)) (f (+ x dx)))))

(define (n-fold-smooth f n)
  ((repeated smoothed n) f))

((n-fold-smooth square 10) 5)
