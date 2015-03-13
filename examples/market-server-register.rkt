#lang racket/base

(require "../spawn.rkt")

(provide market/subscribe
         market/subs/count
         market/subs/apply)

(define market/registrations (make-hash)) ; The "Database".

(define (market/subscribe symbols curl)
  (for-each (lambda (symbol) (hash-set! market/registrations symbol curl)) symbols))

(define (market/subs/count) ; Returns the number of entries in the DB.
  (hash-count market/registrations))

(define (market/subs/apply proc)
  (hash-for-each market/registrations proc))