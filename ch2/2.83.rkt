#lang sicp

(define (raise-pkg)
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real x)
    (make-real (exact->inexact (/ (numer x) (denom x)))))
  (define (real->complex r)
    (make-complex-from-real-imag r 0))
  (put 'raise 'interger integer->rational)
  (put 'raise 'rational rational->real)
  (put 'raise 'real real->complex))

(define (raise x)
  (apply-generic 'raise x))
