#lang sicp

(define (inc x)
  (+ 1 x))

(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (acc-i combiner null-value term a next b)
  (define (iter i result)
    (if (> i b)
        result
        (iter (next i) (combiner result (term i)))))
  (iter a null-value))

(define (acc-i-sum a b)
  (acc-i + 0 identity a inc b))

(define (acc-sum a b)
  (accumulate + 0 identity a inc b))

(define (acc-i-prod a b)
  (acc-i * 1 identity a inc b))

(define (acc-prod a b)
  (accumulate * 1 identity a inc b))

(acc-sum 1 10)
(acc-prod 1 10)

(acc-i-sum 1 10)
(acc-i-prod 1 10)
