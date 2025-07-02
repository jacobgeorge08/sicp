#lang sicp

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

(define x (make-point 2 3))
(define y (make-point 12 13))

(define (perimeter rect)
  (+ (* 2 (car rect)) (* 2 (cdr rect))))
(define (area rect)
  (* (car rect) (cdr rect)))

(define (segment-rectangle l-seg b-seg)
  (let ((l (abs (- (x-point (end-segment l-seg))
                   (x-point (start-segment l-seg)))))
        (b (abs (- (y-point (end-segment b-seg))
                   (y-point (start-segment b-seg))))))
    (cons l b)))

(define seg1 (make-segment (make-point 1 2) (make-point 6 2)))
(define seg2 (make-segment (make-point 6 2) (make-point 6 5)))

(define seg-rect (segment-rectangle seg1 seg2))

(define (point-rectange l-start-p l-end-p b-start-p b-end-p)
  (let ((l (- (x-point l-end-p) (x-point l-start-p)))
        (b (- (y-point b-end-p) (y-point b-start-p))))
    (cons l b)))

(define point-rect (point-rectange (make-point 1 2)(make-point 6 2)(make-point 6 2)(make-point 6 5)))

(perimeter point-rect)
(area point-rect)

