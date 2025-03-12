#lang sicp

(define (d i)
  (if (not (= 0 (remainder (+ i 1) 3)))
      1
      (* 2 (/ (+ i 1) 3))))


(define (cont-frac n d k)
  (define (cont-frac-helper c)
    (if (= k c)
        (/(n c) (d c))
        (/(n c) (+ (d c)(cont-frac-helper (+ 1 c))))))
  (cont-frac-helper 1))

(define e
  (+ 2 (cont-frac (lambda (i) 1.0) d 10)))

e
