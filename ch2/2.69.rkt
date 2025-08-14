#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pair-set)
  (if (null? (cdr pair-set))
      (car pair-set)
      (successive-merge
       (adjoin-set (make-code-tree (car pair-set) (cadr pair-set))
                   (cddr pair-set)))))

(generate-huffman-tree '((A 4)(B 2)(D 1)(C 1)))

;; We have a set of pairs.
;; We want to generate a huffman tree
;; Take the two lowest weights and combine them 
;; and add them to the sorted list.
;; Lot of confusion in this problem because I assumed that adjoin
;; only works for adding a leaf to a list of leaves in the right position.
;; But its actually quite simple because we just keep calling adjoin
;; until we have only one node ie the huffman tree


