#lang sicp

(define (expmod-louis base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m)) ; (expmod base (/ exp 2) m) is eval twice 
                    m))
        (else
         (remainder (* base
                       (expmod base (- exp 1) m))
                    m))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

; We perform the expmod computation twice every time we are trying to calc the square for Louis
; This leads to his version being slower
