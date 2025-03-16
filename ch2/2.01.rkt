#lang racket

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cond ((< d 0)
           (cons (* (/ n g) -1) (* (/ d g) -1)))
          (else (cons (/ n g) (/ d g))))))

(print-rat (make-rat  2 -4))
(print-rat (make-rat -2  4))
(print-rat (make-rat  4 -2))
(print-rat (make-rat -4  2))

