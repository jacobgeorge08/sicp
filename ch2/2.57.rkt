#lang sicp

(define (accumuluate op init seq)
  (if (null? seq)
      init
      (op (car seq) (accumuluate op init (cdr seq)))))

(define (variable? x) (symbol? x))
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

(define (=number? exp y)
  (and (number? exp) (= exp y)))

(define (sum? exp)
  (and (pair? exp) (eq? (car exp) '+)))
(define (addend exp)
  (cadr exp))

;; My initial solution
;; (define (augend exp)
;;   (if (< (length exp) 4)
;;       (caddr exp)
;;       (cons '+ (cddr exp))))

;; A much more elegant solution using accumulate
(define (augend exp)
  (accumuluate make-sum 0 (cddr exp)))


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (product? exp)
  (and (pair? exp) (eq? (car exp) '*)))
(define (multiplier exp)
  (cadr exp))

;; Initial Naive solution
;; (define (multiplicand exp)
;;   (if (< (length exp) 4)
;;       (caddr exp)
;;       (cons '* (cddr exp))))

;; Much more elegant multiplicand
(define (multiplicand exp)
  (accumuluate make-product 1 (cddr exp)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base exp)
  (cadr exp))
(define (exponent exp)
  (caddr exp))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-product
                        (exponent exp)
                        (make-exponentiation (base exp) (- (exponent exp) 1)))
                       (deriv (base exp) var))
         )
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '(* x y (+ x 3)) 'x)
