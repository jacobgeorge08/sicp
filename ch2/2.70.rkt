#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (equal? (car object) 'leaf))
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

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (if (leaf? tree)
      (if (equal? symbol (symbol-leaf tree))
          '()
          #f)
      (let ((left-path (encode-symbol symbol (left-branch tree))))
        (if left-path
            (cons 0 left-path)
            (let ((right-path (encode-symbol symbol (right-branch tree))))
              (if right-path
                  (cons 1 right-path)
                  #f))))))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pair-set)
  (if (null? (cdr pair-set))
      (car pair-set)
      (successive-merge
       (adjoin-set (make-code-tree (car pair-set) (cadr pair-set))
                   (cddr pair-set)))))

(define lyric-pair '((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1)))

(define lyric-tree (generate-huffman-tree lyric-pair))
(define song '(GET A JOB
                   SHA NA NA NA NA NA NA NA NA
                   GET A JOB SHA NA NA NA NA NA NA NA NA
                   WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                   SHA BOOM))

(encode song lyric-tree)
;; => '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 
;;      1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 
;;      0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 1) 
(length (encode song lyric-tree))
;; => 84 bits are required to encode the song

;; Since there are eight symbols in the alphabet, a fixed length encoding
;; would require log(2, 8) = 3 bits per symbol
;; There are 36 symbols in th song. So fixed length encoding would be 108 bits
;; The Huffman Encoding of the song is 22% more efficient

