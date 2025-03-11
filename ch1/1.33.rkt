#lang sicp

(define (prime? n)
  (cond ((= 1 n) #f)
        (else (= n (smallest-divisor n)))))
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

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (filtered-accumulate combiner null-value term a next b filter)
  (cond ((> a b) null-value)
        ((filter a)(combiner (term a) (filtered-accumulate combiner null-value term (next a) next b filter)))
        (else (filtered-accumulate combiner null-value term (next a) next b filter))))

(define (filter-acc-i combiner null-value term a next b filter)
  (define (iter i result)
    (cond((> i b) result)
         ((filter i) (iter (next i) (combiner result (term i))))
         (else (iter (next i) result))))
  (iter a null-value))


(define (sum-prime-squares a b)
  (filter-acc-i + 0 square a inc b prime?))

(sum-prime-squares 1 10)

(define (co-prime-product a b)
  (define (co-prime a)
    (if (= 1 (gcd a b))
        #t
        #f))
  (filter-acc-i * 1 identity a inc b co-prime))

(co-prime-product 1 50)

