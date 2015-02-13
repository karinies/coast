#lang racket/base

(require
 racket/contract/base
 (only-in "../transport.rkt" transport)
 "../channels/bankers.rkt")

(provide
 transport:bankers?
 (contract-out
  [transport:bankers/new (-> transport:bankers?)]))

(struct transport:bankers transport ())

(define (transport:bankers/new)
  (transport:bankers
   (channel:bankers/new)
   (make-semaphore 1)    ; exclusion
   (make-semaphore 0)    ; trigger
   0 0 0 0))             ; sends receives overflows underflows

