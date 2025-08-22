#lang sicp

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types: APPLY-GENERIC"
           (list op type-tags))))))

;; How this works is we have magnitude. As well complex number
;; (magnitude z) invokes app-generic 
;; from apply-generic, type-tags of (magnitude z) gives us 'complex as the type tag
;; we now look in the complex table and have (mag ('rectangular z))
;; The complex tag has been stripped away
;; apply-generic is called again
;; type-tag is now rectangular
;; proc becomes the procedure returned by 'mag 'rectangular 
;; apply mag on (contents z)
;; giving us 5
;; So apply generic is invoked twice, 
;; in the first case apply generic is called again 
;; And again when mag is called from the table corresponding 
;; to rectangle in the complex number package
;; number pacakge 
