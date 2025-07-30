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

(define (has op expr)
  (and (pair? expr) (memq op expr)))

(define (unwrap expr)
  (if (and (pair? expr) (null? (cdr expr)))
      (car expr)
      expr))

(define (before op expr)
  (define (iter expr)
    (if (eq? op (car expr))
        '()
        (cons (car expr) (iter (cdr expr)))))
  (unwrap (iter expr)))

(define (after op expr)
  (unwrap (cdr (memq op expr))))

(define (sum? expr) (has '+ expr))
(define (addend expr) (before '+ expr))
(define (augend expr) (after '+ expr))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (product? expr) (and (not (sum? expr)) (has '* expr)))
(define (multiplier expr) (before '* expr))
(define (multiplicand expr) (after '* expr))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

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
        (else
         (error "unknown expression type: DERIV" exp))))

(deriv '(x + (3 * (x + (y + 2)))) 'x)
(deriv '(x + 3 * (x + y + 2)) 'x)

