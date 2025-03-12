#lang sicp

;; 1/golden ratio
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))
(/ 1 (fixed-point (lambda (x) (+ 1 (/ 1 x))) 2.0))


(define (cont-frac n d k)
  (define (cont-frac-helper c)
    (if (= k c)
        (/(n c) (d c))
        (/(n c) (+ (d c)(cont-frac-helper (+ 1 c))))))
  (cont-frac-helper 1))

(define (always-one x)
  1.0)
(cont-frac always-one always-one 11)
;; Takes 11 iterations to get 4 deciamal places accuracy


(define (cont-iter n d k)
  (define (iter c result)
    (if (= 0 c)
        result
        (iter (- c 1)(/ (n c)(+ (d c) result)))))
  (iter k 0))
(cont-iter always-one always-one 11)
