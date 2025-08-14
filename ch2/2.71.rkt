#lang sicp

;; Had to consult mk12 for this one 

;; Suppose we have a Huffman tree for an alphabet of n symbols,
;; and that the relative frequencies of the symbols
;; are 1, 2, 4, . . . , 2^(n −1). Sketch the tree for
;; n = 5; for n = 10. In such a tree (for general n)
;; how many bits are required to encode the most frequent symbol?
;; The least frequent symbol?

;; We get a slanting tree with every right branch going to a leaf
;; This is because at each stage the weight of the tree is less
;; than the weight of the next leaf 
