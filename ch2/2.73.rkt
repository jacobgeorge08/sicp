#lang sicp

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;; There are general symbols for numbers or variables and so we have to stick to explicit
;; dispatch

(define (install-sum-and-product)
  (define (deriv-sum args var)
    (make-sum (deriv (car args) var)
              (deriv (cadr args) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (install-product)
  (define (deriv-product args var)
    (make-sum
     (make-product (car args) (deriv (cadr args) var))
     (make-product (deriv (car args) var) (cadr args))))
  (put 'deriv '* deriv-product)
  'done)

(define (install-expt)
  (define (deriv-expt args var)
    (let ((b (car args))
          (e (cadr args)))
      (make-product e (make-product (make-exponentiation b (make-sum e -1))
                                    (deriv b var)))))
  (put 'deriv '** deriv-expt)
  'done)

;; ((get (operator exp) 'deriv) (operands exp) var)
;; If this was the order we wanted to <get> tags in we just need to swap the order of our <put>
