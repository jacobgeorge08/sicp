#lang sicp

; n < 3 => n
; n >= 3  => f(n-1) + 2f(n-2) + 3f(n-3)
(define (f-recur n)
  (if (< n 3)
      n
      (+
       (f-recur(- n 1))
       (* 2 (f-recur(- n 2)))
       (* 3 (f-recur(- n 3)))
       )))
(f-recur 3)


(define (f-iterative n)
  (define (f-loop a b c counter)
    (if (= counter n) a
        (f-loop (+ a (* 2 b) (* 3 c))
                a
                b
                (+ counter 1))))
  (if (< n 3) n
      (f-loop 2 1 0 2)))
(f-iterative 40)
; Need help to grok iterative solution

