#lang racket/base

(require "../../getters.rkt"
         "../examples-base.rkt")

(provide market/subscribe
         market/subs/count
         market/subs/apply-all
         market/subs/apply
         market-event
         market-event/symbol
         market-event/type
         market-event/price
         market-event/quantity
         market-event/seller
         market-event/buyer
         )

(provide vector->market-event)

(struct market-event
  (symbol    ; Stock symbol.
   type      ; The event type.
   price     ; The share price in cents.
   quantity  ; The amount of shares that are traded.
   seller    ; The seller ID.
   buyer)    ; The buyer ID.
  #:transparent)

(struct/getters/define market-event symbol type price quantity seller buyer)

(define (vector->market-event v)
  (if (equal? (vector-ref v 0) 'struct:market-event)
      (let ([symbol (vector-ref v 1)]
           [type (vector-ref v 2)]
           [price (string->number(vector-ref v 3))]
           [quantity (string->number(vector-ref v 4))]
           [seller (vector-ref v 5)]
           [buyer (vector-ref v 6)])
       (market-event symbol type price quantity buyer seller))
      (islet/log/error "VECTOR IS NOT MARKET EVENT\n")))
   
(define market/registrations (make-hash)) ; The "Database" for market event registrations

(define (market/subscribe symbols curl)
  (for-each
    (lambda (symbol)
      (if (hash-has-key? market/registrations symbol) ; is there already a key for this symbol?
          ; get the curl list, append new curl, update hash
          (hash-set! market/registrations symbol (append(hash-ref market/registrations symbol)(list curl)))
          ; otherwise, make a new list with single curl
          (hash-set! market/registrations symbol (list curl))))
    symbols))


(define (market/subs/count) ; Returns the number of entries in the DB.
  (hash-count market/registrations))

(define (market/subs/apply-all proc) ; apply proc to all subs passing key and value
  (hash-for-each market/registrations proc))

(define (market/subs/apply event proc) ; apply proc to only subs on symbol
  (when (hash-has-key? market/registrations (market-event-symbol event))
    (for-each (lambda (curl)
       (proc event curl))
       (hash-ref market/registrations (market-event-symbol event)))))