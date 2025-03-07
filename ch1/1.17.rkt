#lang sicp

(define (double x)
  (* 2 x))

(define (half x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))

(define (fast* a b)
  (cond ((= b 1) a)
        ((or (= a 0) (= b 0)) 0)
        ((even? b) (fast* (double a) (half b)))
        (else (+ a (fast* a (- b 1))))))

(fast* 2 100)

