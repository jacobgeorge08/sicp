#lang sicp
;; Exercise 2.54: Two lists are said to be equal? if they contain equal elements arranged in the same order. For example,

(define (equal? list1 list2)
  (cond ((or (not (pair? list1)) (not (pair? list2)))
          (eq? list1 list2))
        (else (and (equal? (car list1) (car list2))
                   (equal? (cdr list1) (cdr list2))))))

(equal? '(this is a list) '(this is a list)) 
(equal? '(this (is a) list) '(this (is a) list)) 
(equal? '(this is a list) '(this (is a) list)) 

