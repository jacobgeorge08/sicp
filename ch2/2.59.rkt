#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (union-set (cdr set1) (adjoin-set (car set1) set2)))))

(define a '(1 2 3 4 5))
(define b '(5 1 2 3 9))

(union-set a b)

;; A nicer solution using accumulate

;; (define (accumulate op init list)
;;   (cond ((null? list) init)
;;         (else (op (car list) (accumulate op init (cdr list))))))
;;
;; (define (union-set* set1 set2)
;;   (accumulate adjoin-set set1 set2))
;;
;; (union-set* a b)
