#lang sicp

(define (fast* a b)
  (cond ((> a b) (mult a b 0))
        ((< a b) (mult b a 0))))

(define (mult a b x)
  (cond ((= b 1) (+ a x))
        ((or (= a 0) (= b 0)) 0)
        ((even? b) (mult (double a) (half b) x))
        (else (mult a (- b 1)(+ a x)))))

(define (double x)
  (* 2 x))

(define (half x)
  (/ x 2))

(define (even? x)
  (= (remainder x 2) 0))


(fast* 324324 123123)
