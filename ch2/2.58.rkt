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
  (and (pair? exp) (eq? (cadr exp) '+)))
(define (addend exp)
  (car exp))

(define (augend exp)
  (accumuluate make-sum 0 (cddr exp)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? exp)
  (and (pair? exp) (eq? (cadr exp) '*)))
(define (multiplier exp)
  (car exp))

(define (multiplicand exp)
  (accumuluate make-product 1 (cddr exp)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

(define (exponentiation? exp)
  (and (pair? exp) (eq? (cadr exp) '**)))
(define (base exp)
  (car exp))
(define (exponent exp)
  (caddr exp))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        ((and (number? b) (number? e)) (expt b e))
        (else (list b '** e))))

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

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)


;; The problem becomes substantially harder if we allow standard algebraic notation, such as (x + 3 * (x + y + 2)), which drops unnecessary parentheses and assumes that multiplication is done before addition. Can you design appropriate predicates, selectors, and constructors for this notation such that our derivative program still works?

;; Im thinking this is just a combination of the previous two questions
;; The first where we remove brackets and the second where we go from
;; prefix to infix form
;; Going to infix form wasnt that difficult since we just change the selectors
;;
;; iterate through the expression and see if theres a *
;; When I wrote an interpreter how we'd do it is,
;; if ( , priority max
;; if * , 2nd 
;; if + , last
