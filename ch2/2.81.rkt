#lang sicp
;; Part A
;; if we call exp on two complex numbers
;; It looks through the type table and finds no procedures for expt
;; It moves to the coercion table and and coerces each complex number to a complex
;; It tries to apply generic exp again to the two complex numbers
;; Again goes to the coercion table and coerces both complex numbers to complex
;; We just get infinite recursion

;; Part B
;; Louis Reasoner's is wrong becuase even when both arguments are the same type and
;; an attempt to coercion happens we just run into an error
;; Explicitly adding coercion to self just results in infinite recursion

;; Part C
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (if (eq? type1 type2)
                    (error "no method for these types" (list op type-tags))
                    (let ((t1->t2 (get-coercion type1 type2))
                          (t2->t1 (get-coercion type2 type1)))
                      (cond (t1->t2
                             (apply-generic op (t1->t2 a1) a2))
                            (t2->t1
                             (apply-generic op a1 (t2->t1 a2)))
                            (else
                             (error "no method for these types"
                                    (list op type-tags)))))))
              (error "no method for these types"
                     (list op type-tags)))))))
