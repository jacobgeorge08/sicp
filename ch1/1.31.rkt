#lang sicp

(define (iden x)
  x)
(define (inc a)
  (+ 1 a))

; This is recurisve procedure for product
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

; Iterative procedure would be
(define (product-i term a next b)
  (define (p-iter a result)
    (if (> a b)
        result
        (p-iter (next a) (* result (term a)))))
  (p-iter a 1))

(define (fact n)
  (product iden 1 inc n))

(define (fact-i n)
  (product-i iden 1 inc n))

; approximation of pi
(define (pi n)
  (define (num i)
    (if (even? i)
        (+ i 2)
        (+ i 1)))
  (define (denom i)
    (if (odd? i)
        (+ i 2)
        (+ i 1)))
  (* 4.0 (/ (product-i num 1 inc n)
          (product-i denom 1 inc n))))

(pi 10000)

