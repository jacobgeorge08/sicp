#lang sicp

(define (prime? n)
  (= n (smallest-divisor n)))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder a b) 0))

(define (square x)
  (* x x))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(define (filter pred sequence)
  ( cond ((null? sequence) nil )
         ((pred (car sequence))
          (cons (car sequence)
                (filter pred (cdr sequence))))
         (else (filter pred (cdr sequence)))))

(define (enumerate-interval n)
  (define (enumerate low high)
    (if (> low high)
        nil
        (cons low (enumerate (+ 1 low) high))))
  (enumerate 1 n))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j)) (enumerate-interval (- i 1))))
           (enumerate-interval n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
