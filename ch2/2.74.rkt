#lang sicp

;; Each division should add a tag to their individual file
;; and implement a get-record that deals with their individual structure

;; We already have a get-record with every divisions tag.
;; Every division just needs to implement a get-salary record

(define (get-salary record)
  (apply-generic 'get-salary (type-tag record) (contents record)))

(define (find-employee-record employee-name files)
  (if (null? files)
      #f
      (or (get-record (car files) employee-name)
          (find-employee-record employee-name (cdr files)))))

;; If they take over a new company, they just need to tag the new-company's file 
;; and install get-record and get-salary
