#lang sicp

(define (p) (p))

(define (test x y)
(if (= x 0) 0 y))

(test 0 (p))

; If we're following applicative order 
; The value of p is evaluated immediately and it keeps calling itself infinitely
; The value is never computed
;
; If we're follwing normal order 
; The value of p is never evaluated and the result of x is 0 
; But p is never computed
