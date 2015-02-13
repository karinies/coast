#lang racket/base

(require
 racket/contract/base
 "../getters.rkt")

(provide
 channel
 channel?
 (contract-out
  [channel/put (channel? any/c . -> . boolean?)]
  [channel/take ((and/c channel? channel/nonempty?) . -> . any/c)]
  [channel/empty? (channel? . -> . boolean?)]
  [channel/nonempty? (channel? . -> . boolean?)]
  [channel/backlog (channel? . -> . exact-nonnegative-integer?)]
  [channel/metrics (-> channel? vector?)]
  
  [channel/new (any/c procedure? procedure? . -> . channel?)]
  [channel/base (channel? . -> . any/c)]
  ;[channel/rebase (channel? any/c . -> . void?)] ; DEPRECATED.
  [channel/base/set (channel? any/c . -> . void?)]
  [channel/backlog++ (channel? . -> . void?)]
  [channel/backlog-- (channel? . -> . void?)]
  [channel/puts   (-> channel? exact-nonnegative-integer?)]
  [channel/puts++ (channel? . -> . void?)]
  [channel/takes  (-> channel? exact-nonnegative-integer?)]
  [channel/takes++ (channel? . -> . void?)]))

;; Abstract underlying channel for transports.
(struct
 channel
 ((base    #:mutable) ; Base object for channel.
  (backlog #:mutable) ; Current backlog of base object.
  (puts    #:mutable) ; Total number of puts to date.
  (takes   #:mutable) ; Total number of takes to date.
  put    ; Base-specific put method (put c x)
  take)) ; Base-specific take method (take c)
(struct/getters/define channel base backlog puts takes)

;; Abstract channel functions.

;; Places value x on channel c.
(define (channel/put c x)     ((channel-put c) c x))
;; Removes and returns a value from nonempty channel c
(define (channel/take c)      ((channel-take c) c))
;; Returns #t if channel c is empty and #f otherwise.
(define (channel/empty? c)    (zero? (channel/backlog c)))
;; Returns #t is channel c is nonempty and #f otherwise.
(define (channel/nonempty? c) (positive? (channel/backlog c)))
;; (channel/backlog c) returns number of values (n >= 0) held by channel c.
;; (channel/puts c) returns total number of puts into channel c.
;; (channel/takes c) returns total number of takes from channel c.

;; Returns an environ containing the current metrics values of channel c.
(define (channel/metrics c)
  (vector-immutable 'channel/metrics 'backlog (channel/backlog c) 'puts (channel/puts c) 'takes (channel/takes c)))
  ;(environ/cons environ/null 'backlog (channel-backlog c) 'puts (channel-puts c) 'takes (channel-takes c)))

;; Constructor
;; base - Functional structure that is the underlying implementation of the channel.
;; put  - base-specific method (put c x) that inserts value x into the base functional structure
;; take - base-specific method (take c x) that removes a value from the base functional structure
(define (channel/new base put take)
  base
  0 0 0 ; puts takes backlog
  put
  take)

;; Utility functions for implementing base-specific channels.
;(define (channel/rebase c b) (set-channel-base! c b)) DEPRECATED.
(define (channel/base/set c b) (set-channel-base! c b))
(define (channel/backlog++ c) (set-channel-backlog! c (add1 (channel-backlog c))))
(define (channel/backlog-- c) (set-channel-backlog! c (sub1 (channel-backlog c))))
(define (channel/puts++ c) (set-channel-puts! c (add1 (channel-puts c))))
(define (channel/takes++ c) (set-channel-takes! c (add1 (channel-takes c))))



