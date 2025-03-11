#lang sicp

(define (square x)
  (* x x))

(define (remainder-square x m)
  (if (and (not (or (= x 1)
                    (= x (- m 1))))
           (= (remainder (square x) m) 1))
      0
      (remainder (* x x) m)))

(define (expmod-miller base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder-square (expmod-miller base (/ exp 2) m) m))
        (else
         (remainder (* base (expmod-miller base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (define (try-it a)
    (= (expmod-miller a (- n 1) n) 1))
  (try-it (+ 1 (random (- n 1)))))

(define (miller-rabin-prime?  n times)
  (cond ((= times 0) #t)
        ((miller-rabin-test n)
         (miller-rabin-prime? n (- times 1)))
        (else #f)))

(miller-rabin-prime? 561 10)
