#lang racket/base

(require
 racket/contract/base
 "channel.rkt"
 "../getters.rkt"
 "../persistent/environ.rkt"
 "../uuid.rkt")

(provide
 transport
 transport?
 (contract-out
  [transport/put (-> transport? any/c boolean?)]
  [transport/take (-> transport? any/c)]
  [transport/lock (-> transport? void?)]
  [transport/unlock (-> transport? void?)]
  [transport/notify (-> transport? void?)]
  [transport/unnotify (-> transport? boolean?)]
  [transport/wait (-> transport? void?)]
  [transport/sends++ (-> transport? void?)]
  [transport/receives++ (-> transport? void?)]
  [transport/overflows++ (-> transport? void?)]
  [transport/underflows++ (-> transport? void?)])
 (rename-out
  [transport-channel transport/channel]
  [transport-trigger transport/trigger])
 (contract-out
  [transport/backlog (-> transport? exact-nonnegative-integer?)]
  [transport/empty? (-> transport? boolean?)]
  [transport/nonempty? (-> transport? boolean?)]
  [transport/metrics (-> transport? vector?)]
  [transport/channel/metrics (-> transport? vector?)]))

;; A transport is the abstraction for intra- and inter-island communication in which
;; distinct forms of transport implement specific communication semantics.
;; In general transmissions are asynchronous and receives are blocking, although there
;; are some transport-specific exceptions.
;; Transports are also the abstraction for a variety of thread synchronization mechanisms.

;; We mainain the invariant that the count of the trigger semaphore
;; is always positive if the channel is nonempty and zero if the
;; channel is empty.
(struct
 transport
 (channel  ; Underlying channel implementation of the transport.
  exclude  ; Semaphore guaranteeing exclusive access among multiple sending and receiving threads.
  trigger  ; Semaphore used to notify threads waiting to receive.
  (sends      #:mutable)   ; Total number of successful sends on channel. 
  (receives   #:mutable)   ; Total number of successful receives on channel.
  (overflows  #:mutable)   ; Total number of failed sends on channel.
  (underflows #:mutable))) ; Total number of receives that blocked.
(struct/getters/define transport channel exclude trigger sends receives overflows underflows)
(struct/setters/define transport sends receives overflows underflows)
;; Utility functions for manipulating transport metrics.
(struct/++/define transport sends receives overflows underflows)
         
;; Returns the backlog (n >= 0) of the underlying channel.
(define (transport/backlog t) (channel/backlog (transport-channel t)))
;; Returns #t if the underlying channel is empty and #f otherwise.
(define (transport/empty? t)  (channel/empty?  (transport-channel t)))
;; Returns #t if the underlying channel is nonempty and #f otherwise.
(define (transport/nonempty? t) (channel/nonempty? (transport-channel t)))

;; Returns an environ containing a snapshot of the transport metrics.
(define (transport/metrics t)
  (vector-immutable
   'transport/metrics
   'sends      (transport-sends t)
   'receives   (transport-receives t)
   'overflows  (transport-overflows t)
   'underflows (transport-underflows t)))
  
;  (environ/cons
;   environ/null
;   'sends      (transport-sends t)
;   'receives   (transport-receives t)
;   'overflows  (transport-overflows t)
;   'underflows (transport-underflows t)))
;; Returns an environ containing a snapshot of metrics of the underlying channel.
(define (transport/channel/metrics t) (channel/metrics (transport-channel t)))

(define (transport/put t x)    (channel/put  (transport-channel t) x))
(define (transport/take t)     (channel/take (transport-channel t)))
(define (transport/lock t)     (semaphore-wait (transport/exclude t)))
(define (transport/unlock t)   (semaphore-post (transport/exclude t)))
(define (transport/notify t)   (semaphore-post      (transport-trigger t)))
(define (transport/unnotify t) (semaphore-try-wait? (transport-trigger t)))
(define (transport/wait t)     (semaphore-wait      (transport-trigger t)))





