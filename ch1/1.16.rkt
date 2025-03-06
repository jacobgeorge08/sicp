#lang sicp

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (even? n)
  (= (remainder n 2) 0))

(define (square x)
  (* x x))

(define (fast-iter b n)
  (fast-i 1 b n))

(define (fast-i a b n)
  (cond ((= n 0) a)
        ((even? n) (fast-i a (square b) (/ n 2)))
        (else (fast-i (* a b) b (- n 1)))))

(fast-iter 5 10)
(fast-expt 5 10)
