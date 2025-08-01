#lang sicp

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? (car set) x) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define a '(2 3 2 1 3 2 8))
(define b '(2 3 2 1 3 2))
(union-set a b)
(intersection-set a b)
(adjoin-set '5 b)

;; If we want to allow duplicates, we only need to change adjoin and union
;; adjoin basically becomes cons and union becomes append

;; Efficiency 
  ;; Adjoin
    ;; Without duplicates its O(n) since we need to scan the whole list
    ;; With duplicates its O(1) since its just cons

  ;; Element of Set
    ;; Without duplicates its O(n)
    ;; With duplicates its also O(n) since we would find the element 
      ;; just as fast. Its just the list that is bigger 

  ;; Intersection of Set
    ;; Without duplicates its O(n^2) since worst case is that for every
      ;; element of set1, we also check every element of set2
    ;; WIth duplicates, it should also be O(n^2) since we still do check
      ;; every element to see if it exists or not 

  ;; Union of Set
    ;; Without duplicates its O(n^2) for the reason as intersection
    ;; With duplicates, the efficiency depends upon append which should
      ;; make it O(n)
