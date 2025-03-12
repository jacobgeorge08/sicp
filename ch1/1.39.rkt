#lang sicp

(define (square x) (* x x))

#| (define pi (3.14159265359)) |#

(define (cont-frac n d k)
  (define (cont-frac-helper c)
    (if (= k c)
        (/(n c) (d c))
        (/(n c) (+ (d c)(cont-frac-helper (+ 1 c))))))
  (cont-frac-helper 1))

(define (tan-cf x k)
  (cont-frac
   (lambda (i) (if (= i 1) x (- (square x))))
   (lambda (i) (- (* i 2) 1))
   k))

(define quarter-pi (atan 1))

(tan-cf quarter-pi 11)
