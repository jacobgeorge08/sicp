#lang sicp

(define (gcd a b)
  (count-gcd a b 0))

(define (count-gcd a b c)
  (display "a: ")(display a)(display " b: ")(display b)(newline)
  (if (= b 0)
      c
      (count-gcd b (remainder a b)(+ 1 c))))

(gcd 206 40)

; Applicative order takes 4 operations 
; Normal order takes 18 operations
