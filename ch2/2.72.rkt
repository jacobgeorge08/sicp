#lang sicp

;; Consider the encoding procedure that you
;; designed in Exercise 2.68. What is the order of growth in
;; the number of steps needed to encode a symbol? Be sure
;; to include the number of steps needed to search the symbol 
;; list at each node encountered. To answer this question
;; in general is difficult. Consider the special case where the
;; relative frequencies of the n symbols are as described in Ex-
;; ercise 2.71, and give the order of growth (as a function of n)
;; of the number of steps needed to encode the most frequent
;; and least frequent symbols in the alphabet.

;; (encode-symbol) from 2.68
;; (define (encode-symbol symbol tree)
;;   (if (leaf? tree)
;;       (if (eq? symbol (symbol-leaf tree))
;;           '()
;;           false)
;;       (let ((left-path (encode-symbol symbol (left-branch tree))))
;;         (if left-path
;;             (cons 0 left-path)
;;             (let ((right-path (encode-symbol symbol (right-branch tree))))
;;               (if right-path
;;                   (cons 1 right-path)
;;                   (error "symbol not found" symbol)))))))

;; To encode the most frequent symbol would be easy, since its only 1 step
;; away and this would be an O(1) operation
;; To encode the least frequent symbol, we would have to traverse the entire
;; list and so we get O(n)
