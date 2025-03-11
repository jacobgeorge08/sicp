#lang sicp

; Prime Fucntion
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


; Using this procedure, write a procedure search-for-primes that checks the primality of consecutive odd integers in a speciﬁed range. Use your procedure to ﬁnd the three smallest primes larger than 1000; larger than 10,000; larger than 100,000; larger than 1,000,000. Note the time needed to test each prime. Since the testing algorithm has order of growth √ of Θ( n), you should expect that testing for primes around √10,000 should take about 10 times as long as testing for primes around 1000. Do your timing data bear this out? How well do the data for 100,000 and 1,000,000 support the √ Θ( n) prediction? Is your result compatible with the notion that programs on your machine run in time proportional to the number of steps required for the computation?


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

