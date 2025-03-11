#lang sicp

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))


(define (sum-i term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))


(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (define (cube x)
    (* x x x))
  (sum cube a inc b))


(define (pi-sum-i a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum-i pi-term a pi-next b))

(define (sum-integers-i a b)
  (sum-i identity a inc b))

(define (sum-cubes-i a b)
  (define (cube x)
    (* x x x))
  (sum-i cube a inc b))


(sum-integers 1 10)
(sum-integers-i 1 10)
(sum-cubes-i 1 10)
(sum-cubes 1 10)
(pi-sum 1 10)
(pi-sum-i 1 10)
