#lang sicp

(define (same-parity . items)
  (let ((parity? (if (even? (car items)) even? odd?)))
    (define (helper items result)
      (cond ((null? items) (reverse result))
            ((parity? (car items)) (helper (cdr items) (cons (car items) result)))
            (else (helper (cdr items) result))))
    (helper items nil)))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)

