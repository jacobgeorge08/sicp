#lang sicp

(define (f-recur n)
  (if (< n 3)
      n
      (+
       (f-recur(- n 1))
       (* 2 (f-recur(- n 2)))
       (* 3 (f-recur(- n 3)))
       )))
(f-recur 3)


(define (f- n)
  (define (f-iter a b c counter)
    (if (= counter n) a
        (f-iter (+ a (* 2 b) (* 3 c))
                a
                b
                (+ counter 1))))
  (if (< n 3) n
      (f-iter 2 1 0 2)))
(f- 40)

