#lang sicp

;; Explicit Dispatch Style
  ;; Need to explicitly add cond statements for every type and every operation each time
  ;;  either is added

;; Data-Directed Style
  ;; When a new type is added, need to add an additional column that has all the operations
  ;; When a new operation is added, need to add an additional row for every single type

;; Message Passing
  ;; When a new type is added, need to write a procedure that handles all operations
  ;; When a new operations is added, need to a clause to the dispatch of all types

;; If new types appear often message-passing minimises the work (only one new procedure per type).  
;; if new operations appear often data-directed minimises the work (only one new column/operation).
