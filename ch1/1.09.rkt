#lang sicp

;Each of the following two procedures deﬁnes a method for adding two positive integers in terms of the procedures inc, which increments its argument by 1, and dec, which decrements its argument by 1. Using the substitution model, illustrate the process generated by each procedure in evaluating (+ 4 5). Are these processes iterative or recursive?

(define (+ a b)
  (if (= a 0) b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0) b
      (+ (dec a) (inc b))))

(+ 4 5)

; The first process is recursive because we can see that the calls to + keep building up, adding one inc every single call. 
; This buildup of deferred calls to inc is the recursive process
; (+ 4 5)
; inc (+ 3 5)
; inc(inc(+ 2 5))
; inc(inc(inc(+ 1 5)))
; inc(inc(inc(inc(+ 0 5))))
; inc(inc(inc(inc(5))))
; inc(inc(inc(6)))
; inc(inc(7))
; inc(8)
; 9

; The second process is iterative and does not generate a stack built up like above. There are no deferred calls. This is tail recursion
; (+ 4 5)
; (+ 3 6)
; (+ 2 7)
; (+ 1 8)
; (+ 0 9)
; 9
