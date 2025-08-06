#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (key record)
  (car record))

(define (record-data record)
  (cdr record))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))
             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))

;; Okay for an invidual record
;; car of the record is the key
;; cdr is the record-data
;; If the tree is ordered by numerical values of the keys
;; That means, its already a BST
;; All we have to do is search through the tree for the
;; right key and return the record
;; This is traversal of a BST
;; answer is either false else record-data

(define (lookup given-key record-tree)
  (cond ((null? record-tree) #f)
        ((equal? given-key (key (entry record-tree)))
         (record-data (entry record-tree)))
        ((< given-key (key (entry record-tree)))
         (lookup given-key (left-branch record-tree)))
        (else
         (lookup given-key (right-branch record-tree)))))

(lookup 3 '((2 water) ((1 flour) () ()) ((3 salt) () ()))) 
(lookup 7 (list->tree '((1 flour) (2 water) (3 salt) (4 yeast))))

