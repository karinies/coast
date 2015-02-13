#lang racket/base

;; A transport channel that rides atop a Racket thread mailbox.

(require
 racket/contract/base
 "../channel.rkt")

(provide
 channel:thread?
 (contract-out
  [channel:thread/new (-> thread? channel:thread?)]))

(struct channel:thread channel ())

(define (channel:thread/put c x)
  (cond
    [(thread-send (channel/base c) x #f)
     (channel/puts++ c)
     (channel/backlog++ c)
     #t]
    [else #f]))

(define (channel:thread/take c x)
  (when (eq? (current-thread) (channel/base c))
    (begin0
      (thread-receive)
      (channel/backlog-- c)
      (channel/takes++ c))))

(define (channel:thread/new t)
  (channel:thread
   t
   0 0 0 ; backlog puts takes
   channel:thread/put
   channel:thread/take))

