#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (make-segment start end)
  (cons start end))
(define (start-segment x)
  (car x))
(define (end-segment x)
  (cdr x))

(define (mid-point seg)
  (let ((mid-start (average (x-point (start-segment seg)) (x-point (end-segment seg))))
        (mid-end (average (y-point (start-segment seg)) (y-point (end-segment seg)))))
    (make-point mid-start mid-end)))

(define x (make-point 6 5))
(define y (make-point 12 13))

(define seg1 (make-segment x y))
seg1
(mid-point seg1)
