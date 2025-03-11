#lang sicp

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
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


(define (prime? n)
  (= n (smallest-divisor n)))
(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))
(define (square x)
  ( * x x))


(define (carmichael)
  (display "561 \n")(display "Fermat Little Test :")(display (fast-prime? 561 10))
  (display "  Prime Test: ") (display (prime? 561))(newline)
  (display "1105  \n")(display "Fermat Little Test :")(display (fast-prime? 1105 10))
  (display "  Prime Test: ") (display (prime? 1105))(newline)
  (display "1729 \n")(display "Fermat Little Test :")(display (fast-prime? 1729 10))
  (display "  Prime Test: ") (display (prime? 1729))(newline)
  (display "2465 \n")(display "Fermat Little Test :")(display (fast-prime? 2465 10))
  (display "  Prime Test: ") (display (prime? 2465))(newline)
  (display "2821 \n")(display "Fermat Little Test :")(display (fast-prime? 2821 10))
  (display "  Prime Test: ") (display (prime? 2821))(newline)
  (display "6601 \n")(display "Fermat Little Test :")(display (fast-prime? 6601 10))
  (display "  Prime Test: ") (display (prime? 6601)))

(carmichael)

; Guess thats why its a probabilistic test/algorithm and we can only be reasonably certain about primes
; Not a 100%
