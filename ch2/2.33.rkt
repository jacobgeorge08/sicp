#lang sicp

(define (square x)
  (* x x))

(define x (list 1 2 3))
(define y (list 4 5 6))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map p sequence)
  (accumulate (lambda (x y)
                (cons (p x) y))
              nil sequence))
(map inc x)

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))
(append x y)

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0 sequence))
(length x)
