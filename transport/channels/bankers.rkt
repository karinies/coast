#lang racket/base

(require
 racket/contract/base
 "../channel.rkt"
 (only-in "../../persistent/queue/bankers.rkt" bankers/null bankers/put bankers/take))

(provide
 channel:bankers?
 (contract-out
  [channel:bankers/new (-> channel:bankers?)]))

;; Channel based on a bankers queue.
(struct channel:bankers channel ())

;; Channel-specific put method.
(define (channel:bankers/put c x)
  (channel/base/set c (bankers/put (channel/base c) x))
  (channel/backlog++ c)
  (channel/puts++ c)
  #t)

;; Channel-specific take method.
(define (channel:bankers/take c)
  (let ((pair (bankers/take (channel/base c))))
    (channel/base/set c (cdr pair))
    (channel/backlog-- c)
    (channel/takes++ c)
    (car pair)))

;; Constructor for bankers queue channel.
(define (channel:bankers/new)
  (channel:bankers
   bankers/null
   0 0 0 ; backlog puts takes
   channel:bankers/put
   channel:bankers/take))