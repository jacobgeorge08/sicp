#lang sicp

; Primes found in 1.22
; Primes > 1000: 1009, 1013, 1019
; Primes > 10,000: 10,007 10,009 10,037
; Primes > 100,000: 100,003 100,019 100,043
; Primes > 1,00,000 : 1000003 1000033 1000037

; Just have to modify the timed-prime procedure to use fermat's fast prime method

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

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (square x)
  ( * x x))


; Prime Timer
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


(define (search-for-primes start end)
  (if (even? start)
      (search-for-primes (+ 1 start) end)
      (cond ((> start end)
             (newline) (display "done")(newline))
            (else (timed-prime-test start)
                  (search-for-primes (+ 2 start) end)))))


(timed-prime-test 1000000021)

; Okay so whats pretty cool is that initially for smaller numbers it looks like the regular test prime is faster
; But as the number we're testing gets much much larger, the sqroot doesnt really help and fast test prime with
; the log n theta stays quite consistent as the input size grows as well as its faster
