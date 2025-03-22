#lang sicp

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (no-more? items)
  (null? items))
(define (first-denomination items)
  (car items))
(define (except-first-denomination items)
  (cdr items))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(cc 19 uk-coins)
(cc 19 (list 50 100 20 5 10 0.5 1 2))
(cc 19 (reverse uk-coins))

;; Doesnt matter what the order is because tree recursion will explore 
;; every possible combination

