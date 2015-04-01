#lang racket/base

(require "../../spawn.rkt"
         "../../getters.rkt"
         )

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

(struct market-event
  (symbol    ; Stock symbol.
   type      ; The event type.
   price     ; The share price in cents.
   quantity  ; The amount of shares that are traded.
   seller    ; The seller ID.
   buyer)    ; The buyer ID.
  #:transparent)

(struct/getters/define
 market-event symbol type price quantity seller buyer)

(define market/registrations (make-hash)) ; The "Database".

(define (market/subscribe symbols curl)
  (for-each (lambda (symbol) (hash-set! market/registrations symbol curl)) symbols))

(define (market/subs/count) ; Returns the number of entries in the DB.
  (hash-count market/registrations))

(define (market/subs/apply-all proc) ; apply proc to all subs passing key and value
  (hash-for-each market/registrations proc))

(define (market/subs/apply event proc) ; apply proc to only subs on symbol
  (define v (hash-ref market/registrations (market-event-symbol event)))
  (proc event v))
