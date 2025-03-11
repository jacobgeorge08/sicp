#lang sicp

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (simpson f a b n)
  (define h (/ (- b a)n))
  (define (add-2h x) (+ x h h))
  (* (+ (f a)
        (* 2 (sum f (add-2h a) add-2h (- b h)))
        (* 4 (sum f (+ a h ) add-2h b))
        (f b))
     (/ h 3)))

(define (cube x)
  (* x x x))


(display (integral cube 0 1 0.01)) (newline)
(display (simpson cube 0 1.0 100)) (newline)
(display (simpson cube 0 1.0 1000)) (newline)
(display (simpson cube 0 1.0 10000)) (newline)


