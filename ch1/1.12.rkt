#lang sicp
;Its easier to visualize the problem when its in row col format
;    1
;    1 1
;    1 2 1
;    1 3 3 1
;    1 4 6 4 1
;    1 5 10 10 5 1
;    1 6 15 20 15 6 1
;    1 7 21 35 35 21 7 1
;
; 0,0
; 1,0 1,1
; 2,0 2,1 2,2
; 3,0 3,1 3,2 3,3

;Calculate an item in pascals triangle
(define (pascal row col)
  (cond ((= row 0) 1)
        ((> col row) 0)
        ((or (= col 0) (= col row)) 1)
        (else (+ (pascal (- row 1) col ) (pascal (- row 1)(- col 1))))))

;Print a row in pascals triangle
(define (print-row n)
  (define (current-col m)
    (display (pascal n m))(display " ")
    (if (= n m) (newline)
        (current-col (+ 1 m))))
  (current-col 0))

;Print triangle
(define (print-pascal row_number)
  (define (triangle counter)
    (print-row counter)
    (if (= counter row_number) (newline)
        (triangle (+ counter 1))))
  (triangle 0))
(print-pascal 7)


