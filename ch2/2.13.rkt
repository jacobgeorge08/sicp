#lang sicp

;; Show that under the assumption of small percentage tolerances there is a simple formula for the approximate percentage tolerance of the product of two intervals in terms of the tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

;; Working it out on pen and paper and while using the fact that 
;; [a,b] * [c,d] = [ac, bd] since all the numbers are positive 
;; As well as using the fact that the tolerances are very small 
;; we get that the approx percentage tolerance when we multiply
;; two intervals x and y = px + py

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

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
  (* (/ (width i) (center i)) 100.0))

(define (make-center-percent c percent-tolerance)
  (let ((tolerance (/ percent-tolerance 100)))
    (make-interval (- c (* c tolerance)) (+ c (* c tolerance)))))

(define a (make-center-percent 5.0 2.0))
(define b (make-center-percent 10.0 3.0))
(define c (mul-interval a b))

(percent c)
;; 4.999 is approx 2.0 + 3.0
