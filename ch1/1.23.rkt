#lang sicp
; Prime Fucntion
(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next-divisor test-divisor)))))

(define (next-divisor n)
  ( if (= n 2) 3
       (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (square x)
  ( * x x))


; Prime Timer
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (if (prime? n)
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

(search-for-primes 1000 1023)
(search-for-primes 10000 10045)
(search-for-primes 100000 100045)
(search-for-primes 1000000 1000050)

(timed-prime-test 1000000021)

; Actually twice as fast. Pretty cool to see
