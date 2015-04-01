#lang racket/base

(require "../../spawn.rkt")

(provide market/subscribe
         market/subs/count
         market/subs/apply-all
         market/subs/apply)

(define market/registrations (make-hash)) ; The "Database".

(define (market/subscribe symbols curl)
  (for-each (lambda (symbol) (hash-set! market/registrations symbol curl)) symbols))

(define (market/subs/count) ; Returns the number of entries in the DB.
  (hash-count market/registrations))

(define (market/subs/apply-all proc) ; apply proc to all subs passing key and value
  (hash-for-each market/registrations proc))

(define (market/subs/apply symbol event-type price proc) ; apply proc to only subs on symbol
  (define v (hash-ref market/registrations symbol))
  (proc symbol event-type price v))
