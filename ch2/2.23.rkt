#lang sicp

(define (for-each proc items)
  (cond ((null? items) (display ""))
        (else (proc (car items))
              (for-each proc (cdr items)))))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))

;; Made assumptions about if and else which led to me struggling with this exercise
;; Okay so the issue was that in my else clause, (inside if), scheme treats it as one argument
;; So we would be trying to apply the result of (proc (car item)) onto the next for each 
;; Whereas in cond, the else clause is treated as multiple arguments that run one by one and the last one is returned 
