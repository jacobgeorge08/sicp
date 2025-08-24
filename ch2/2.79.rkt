#lang sicp

(define (install-equals-package)
  (put 'equ? '(scheme-number scheme-number) = )
  (put 'equ? '(rational-number rational-number)
       (lambda (x y)
         (and (= (numer x) (number y))
              (= (denom x) (denom y)))))
  (put 'equ? '(complex complex)
       (lambda (x y)
         (and (= (real-part x) (real-part y))
              (= (imag-part x) (imag-part y))))))

;; How the equals package works is 
;; If its a scheme number, return the scheme = procedure
;; If its a rational number, compare numer and denom of both x and y
;; If its a complex number, compare real and imaginary parts of both numbers
