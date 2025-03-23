#lang sicp

(define (reverse-recur items)
  (if (null? items)
      nil
      (append (reverse-recur (cdr items)) (list (car items)))))

(define (reverse items)
  (define (reverse-iter remaining result)
    (if (null? remaining)
        result
        (reverse-iter (cdr remaining) (cons (car remaining) result))))
  (reverse-iter items nil))

(reverse (list 1 4 9 16 25))
(reverse-recur (list 1 4 9 16 25))
