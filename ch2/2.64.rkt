#lang sicp

(define (make-tree entry left right)
  (list entry left right))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
               (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result
                   (partial-tree
                    (cdr non-left-elts)
                    right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts
                     (cdr right-result)))
                (cons (make-tree this-entry
                                 left-tree
                                 right-tree)
                      remaining-elts))))))))

(list->tree (list 1 3 5 7 9 11))

;; Okay so how this procedure works is by taking an ordered list and returning
;; a Binary Search Tree 
;; It works by first calculating the size of the left-subtree which is 
;; (n - 1)/2 We subtract 1 since we arent counting the root node 
;; We then call partial-tree again on this left-side 
;; again figuring out the number of elements that go on the left-side of the 
;;  sub-tree's subtree and right side as well 
;; The car of the elements not consumed is the entry node and the cdr is passed
;;  to the right sub-tree. We then call cons with (make-tree) and the elements
;;  that havent been used yet

;;    5
;;   / \
;;  /   \
;; 1     9
;;  \   / \
;;   3 7  11

;; Because each element is visited exactly once and operations like car and cdr are constant time in Scheme, the entire tree is constructed in O(n) time, with no repeated traversal or copying



