#lang racket/base

(require
 racket/contract/base
 (only-in "../transport.rkt" transport)
 "../channels/promise.rkt")

(provide
 transport:promise?
 (contract-out
  [transport:promise/new (-> transport:promise?)]))

(struct transport:promise transport ())

(define (transport:promise/new)
  (transport:promise
   (channel:promise/new)
   (make-semaphore 1)    ; exclusion
   (make-semaphore 0)    ; trigger
   0 0 0 0))             ; sends receives overflows underflows
