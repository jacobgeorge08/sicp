#lang sicp

(define (make-interval a b) (cons a b))
(define (lower-bound x)
  (car x))
(define (upper-bound x)
  (cdr x))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

(define (make-center-percent c percent-tolerance)
  (let ((tolerance (/ percent-tolerance 100)))
    (make-interval (- c (* c tolerance)) (+ c (* c tolerance)))))

(define a (make-center-percent 100 4))
(define b (make-center-percent 200 3))

(define aa (div-interval a a))
(define ab (div-interval a b))
aa
ab
(center aa)

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(par1 a a)
(par2 a a)

(par1 a b)
(par2 a b)

