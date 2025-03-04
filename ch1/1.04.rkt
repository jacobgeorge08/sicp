#lang sicp
; Exercise 1.4: Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Pretty cool that you can have the result of an if expression eval to an operator
; if b > 0 => a + b 
; if b < 0 => a - b => a + b (since b is negative)

