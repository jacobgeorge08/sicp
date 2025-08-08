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

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

;; (define (encode-symbol symbol tree)
;;   (cond ((leaf? tree) '())
;;         ((memq symbol (symbols (left-branch tree)))
;;          (cons 0 (encode-symbol symbol (left-branch tree))))
;;         ((memq symbol (symbols (right-branch tree)))
;;          (cons 1 (encode-symbol symbol (right-branch tree))))
;;         (else (error "symbol doesn't exist" symbol))))
;; How I solved problem initially
;; But this is no more efficient than going through a list and
;; does not use all the benefits that a tee provides us with

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (eq? symbol (symbol-leaf tree))
          '()
          false)
      (let ((left-path (encode-symbol symbol (left-branch tree))))
        (if left-path
            (cons 0 left-path)
            (let ((right-path (encode-symbol symbol (right-branch tree))))
              (if right-path
                  (cons 1 right-path)
                  (error "symbol not found" symbol)))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree (make-leaf 'B 2)
                                  (make-code-tree (make-leaf 'D 1)
                                                  (make-leaf 'C 1)))))
(define msg '(A D A B B C A))
(define invalid-msg '(A B C D E))
(encode msg sample-tree) ;; => (0 1 1 0 0 1 0 1 0 1 1 1 0)
(encode invalid-msg sample-tree) ;; => symbol not found E
