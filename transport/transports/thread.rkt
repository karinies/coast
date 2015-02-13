#lang racket/base

(require
 racket/contract/base
 "../transport.rkt"
 "../channels/thread.rkt")

(provide
 transport:thread?
 (contract-out
  [transport:thread/new (-> transport:thread?)]))

(struct transport:thread transport ())

(define (transport:thread/new)
  (transport:thread
   (channel:thread/new)
   (make-semaphore 1)    ; exclusion
   (make-semaphore 0)    ; trigger
   0 0 0 0))             ; sends receives overflows underflows
