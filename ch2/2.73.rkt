#lang sicp

;; (define (deriv exp var)
;;   (cond ((number? exp) 0)
;;         ((variable? exp)
;;          (if (same-variable? exp var) 1 0))
;;         ((sum? exp)
;;          (make-sum (deriv (addend exp) var)
;;                    (deriv (augend exp) var)))
;;         ((product? exp)
;;          (make-sum (make-product
;;                     (multiplier exp)
;;                     (deriv (multiplicand exp) var))
;;                    (make-product
;;                     (deriv (multiplier exp) var)
;;                     (multiplicand exp))))
;;         ;; ⟨more rules can be added here⟩
;;         (else (error "unknown expression type: DERIV" exp))))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; What happens here is that we have a table of expressions and the get procedure
;; looks up the table to find the + or * procedure and applies it to the operands
;; We cant assimilate number and variable into the table because we have different
;; types of variables not just 1 type and neither number nor variable have a 'tag'
;; that we could recognize them with. We could write the number procedure but this
;; tedious

(define (install-sum)
  (define (deriv-sum terms var)
    (accumulate make-sum 0 (map (lambda (t) (deriv t var)) terms)))
  (put 'deriv '+ deriv-sum))

(define (install-product)
  (define multiplier car)
  (define (multiplicand product)
    (accumulate make-product 1 (cdr product)))
  (define (deriv-product product var)
    (make-sum (make-product (multiplier product)
                            (deriv (multiplicand product) var))
              (make-product (deriv (multiplier product) var)
                            (multiplicand product))))
  (put 'deriv '* deriv-product))

(define (install-exponent)
  (define base car)
  (define exponent cadr)
  (define (deriv-power power var)
    (make-product
     (make-product (exponent power)
                   (make-exponentiation
                    (base power)
                    (make-sum (exponent power) -1)))
     (deriv (base power) var)))
  (put 'deriv '** deriv-power))

;; ((get (operator exp) 'deriv) (operands exp) var)
;; If this was the order we wanted to <get> tags in we just need to swap the order of our <put>
