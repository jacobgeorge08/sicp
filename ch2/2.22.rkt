#lang sicp
;; Exercise 2.22: Louis Reasoner tries to rewrite the ﬁrst square- list procedure of Exercise 2.21 so that it evolves an itera- tive process:
(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter items nil))

;; The answer here is in reverse because we construct a new list every iteration by add to the front of the list. The first element of the list is the square of car things so as we move down the list, the last element will be first and the list will be in reverse

(define (square-list items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter items nil))

;; For the 2nd half of the question, we are constructing a pair with a bigger and bigger initial element and we get this weird dotted notation nested mess
;;
;; While for the first answer we are constructing a list by consing an element with a list like we were meant to do
;; a list is just cons glued together btw
