#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))

(define a (make-interval 101.0 107.0))
(define b (make-interval 25.0 30.0))
(define c (make-interval 45.0 50.0))

(width b)
(width c)

;; b and c have the same widths
;; But when we multiply both with a we get intervals of different widths

(define (mul-width)
  (= (width (mul-interval a c)) (width (mul-interval a b))))

(define (sum-width)
  (= (width (add-interval a c)) (width (add-interval a b))))

(mul-width)
(sum-width)

;; Sum on the other hand gives us the same widths
