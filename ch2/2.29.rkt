#lang sicp

(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (if (pair? (branch-structure branch))
      (total-weight (branch-structure branch))
      (branch-structure branch)))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile)) (branch-weight (right-branch mobile))))

(define (balanced? mobile)
  (and (= (torque (left-branch mobile))
          (torque (right-branch mobile)))
       (balanced-branch (left-branch mobile))
       (balanced-branch (right-branch mobile))))

(define (balanced-branch branch)
  (if (pair? (branch-structure branch))
      (balanced? (branch-structure branch))
      #t))


(define a (make-mobile (make-branch 2 3) (make-branch 2 3)))
(define b (make-mobile (make-branch 4 3) (make-branch 2 3)))
(balanced? a)
(balanced? b)

;; Suppose we change the representation of mobiles so that the constructors are
(define (make-mobile-new left right) (cons left right))
(define (make-branch-new length structure)
  (cons length structure))

;; The only difference is we're using cons instead of a list to glue elements together
;; The first element aka the left branch and the length will be the same and wont need any change
;; The left branch and the structure will have to be accessed with cdr instead of cadr
