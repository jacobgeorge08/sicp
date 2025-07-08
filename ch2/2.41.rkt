#lang sicp

;; Was stuck for a minute because I was trying to define the predicate 
;; for filter but doing that would make it the sequence as well as 
;; Sum S but it's so much easier to just define it as a lambda

;; Other than that, pretty glad I have a better understanding of 
;; accumulate and flatmap now 

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (enumerate n)
  (define (enumerate-interval low high)
    (if (> low high)
        nil
        (cons low (enumerate-interval (+ low 1) high))))
  (enumerate-interval 1 n))

(define (ordered-triplet n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k)
                               (list i j k))
                             (enumerate (- j 1))))
                      (enumerate (- i 1))))
           (enumerate n)))

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (make-trip-sum seq)
  (list (car seq) (cadr seq) (cadr (cdr seq)) (+
                                               (car seq)
                                               (cadr seq)
                                               (cadr (cdr seq)))))

(define (ordered-triplet-sum n s)
  (map make-trip-sum (filter (lambda (el)
                               (= (+ (car el)
                                     (cadr el)
                                     (cadr (cdr el)))
                                  s))
                             (ordered-triplet n))))

(ordered-triplet-sum 6 14)
