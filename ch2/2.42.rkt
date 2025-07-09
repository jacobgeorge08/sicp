#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (filter pred seq)
  (cond ((null? seq) nil)
        ((pred (car seq))
         (cons (car seq)
               (filter pred (cdr seq))))
        (else (filter pred (cdr seq)))))

(define (accumulate op init seq)
  (if (null? seq)
      init
      (op (car seq)
          (accumulate op init (cdr seq)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (attacks? q1 q2)
  (let ((r1 (car q1)) (c1 (cadr q1))
                      (r2 (car q2)) (c2 (cadr q2)))
    (or (= r1 r2)
        (= (abs (- r1 r2)) (abs (- c1 c2))))))

(define (safe? k positions)
  (let ((new-queen (car positions))
        (other-queens (cdr positions)))
    (define (iter remaining-queens)
      (cond ((null? remaining-queens) #t)
            ((attacks? new-queen (car remaining-queens)) #f)
            (else (iter (cdr remaining-queens)))))
    (iter other-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(queens 4)
