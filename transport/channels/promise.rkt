#lang racket/base

;; Used to implement transports that act as promises.

(require
 racket/contract/base
 "../channel.rkt")

(provide
 channel:promise?
 (contract-out
  [channel:promise/new (-> channel:promise?)]))

;; NOTE: We use the channel puts counter to indicate whether or
;; not the promise has been fulfilled.
;; Once fulfilled, both the puts and backlog counters
;; of the promise channel remain at 1 from that point forward.
(struct channel:promise channel ())

(define (channel:promise/new)
  (channel:promise
   #f    ; base (used to store the eventual promise value)
   0 0 0 ; backlog puts takes
   channel:promise/put
   channel:promise/take))

;; Channel-specific put method.
(define (channel:promise/put c x)
  (if (zero? (channel/puts c))
      (begin
        (channel/base/set c x) ; Capture value x that fufills the promise.
        (channel/backlog++ c)  ; Stays fixed at at 1 from this point forward.
        (channel/puts++ c)     ; Nonzero indicates that promise is satisfied.
        #t)
      #f)) ; The promise has already been fulfilled.

;; Channel-specific take method.
(define (channel:promise/take c)
  ; Note: we never decrement the backlog counter to allow unlimited takes from the channel.
  (channel/takes++ c) ; Indicates how many times the promise has been read.
  (channel/base c))



