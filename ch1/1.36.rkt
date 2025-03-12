#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (display "guess :")(display guess)(display " next: ")(display next)(newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0)

(fixed-point (lambda (x) (/ (log 1000) (log x))) 5) ;; 28 steps without damping
(fixed-point (lambda (x) (average  x (/ (log 1000) (log x)))) 5) ;; 8 steps with damping

