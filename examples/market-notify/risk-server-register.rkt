#lang racket/base

(require "../../spawn.rkt"
         "../../getters.rkt"
         )

(provide risk/subscribe
         risk/subs/count
         risk/subs/apply
         risk-event
         risk-event/symbol
         risk-event/type
         risk-event/risk
         )

(struct risk-event
  (symbol    ; Stock symbol.
   type      ; The risk event type.
   risk )    ; Curent risk assessment value.
  #:transparent)

(struct/getters/define risk-event symbol type risk)

(define risk/registrations (make-hash)) ; The "Database" for risk event registrations

(define (risk/subscribe symbols curl)
  (for-each
    (lambda (symbol)
      (cond 
        [(hash-has-key? risk/registrations symbol) ; is there already a key for this symbol?
          ; get the curl list, append new curl, update hash
         (hash-set! risk/registrations symbol (append(hash-ref risk/registrations symbol)(list curl)))]
        [else (hash-set! risk/registrations symbol (list curl))])) ; otherwise, make a new list with single curl
      symbols))

(define (risk/subs/count) ; Returns the number of entries in the DB.
  (hash-count risk/registrations))

(define (risk/subs/apply event proc) ; apply proc to only subs on symbol
  (cond 
    [(hash-has-key? risk/registrations (risk-event-symbol event))
     ; loop through curl list on hash calling (proc event curl) for each
     (for-each (lambda (curl)
       (proc event curl))
       (hash-ref risk/registrations (risk-event-symbol event)))]
    [else (displayln (format "Unregistered symbol ~a" (risk-event-symbol event)))]))
