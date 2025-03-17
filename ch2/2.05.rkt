#lang sicp

(define (cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (car num)
  (define (power num counter base)
    (if (not (= (remainder num base) 0))
        counter
        (power (/ num base) (+ 1 counter) base)))
  (power num 0 2))

(define (cdr num)
  (define (power num counter base)
    (if (not (= (remainder num base) 0))
        counter
        (power (/ num base) (+ 1 counter) base)))
  (power num 0 3))

(car (cons 4 5))
(cdr (cons 4 5))

