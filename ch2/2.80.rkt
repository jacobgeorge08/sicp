#lang sicp

(define (install-equals-package)
  (put '=zero? '(scheme-number)
       (lamda (x)(= 0 x)))
  (put '=zero? '(rational-number)
       (lambda (x)
         (= 0 (numer x))))
  (put '=zero? '(complex complex)
       (lambda (x)
         (and (= 0 (real-part x))
              (= 0 (imag-part x))))))
