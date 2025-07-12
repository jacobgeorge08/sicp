#lang sicp

;; Eva Lu Ator types to the interpreter the expression (car ''abracadabra)
;; To her surprise, the interpreter prints back quote. Explain.


;; The interpreter sees ' as quote 
;; So when type 'abracadbra what the interpreter sees is really 
(quote abracadabra)
;; So ''abracadabra would be interpreted as 
(quote (quote abracadabra))
;; which gives us the symbolic list of (quote abracadabra)
;; the car of which would be quote
