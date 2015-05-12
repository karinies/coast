#lang racket/base

(require racket/contract
         json)

(provide (contract-out
          [como:log/event (-> como:logger? como:event? any/c)]
          [como:log (->* (como:logger? #:source string? #:type string? #:version number? #:value any/c) any/c)]
          [como:event->jsexpr (-> como:event? jsexpr?)]
          [como:transport-messenger/shutdown (-> como:transport-messenger? void?)])
         (struct-out como:transport-messenger)
         (struct-out como:logger)
         (struct-out como:event)
         como:protocol/0.1
         como:protocol/LATEST)

#|
 | Accountability Logging.
 |#
(struct como:event
  (source
   type
   protocol-version
   value
   time)
  )

(struct como:transport-messenger
  (sender
   stopper)
  #:transparent)

(define (como:transport-messenger/shutdown messenger)
  ((como:transport-messenger-stopper messenger)))

(struct como:logger
  (messenger)
  #:transparent)

(define (como:log/event logger event)
  (displayln (format "Logging event ~a using ~a" event (como:logger-messenger logger)))
  (let* ([messenger (como:logger-messenger logger)]
         [sender (como:transport-messenger-sender messenger)])
    (sender event)))

(define (como:log logger #:source source #:type type #:version version #:value value)
  (let ([event (como:event source type version value (current-inexact-milliseconds))])
    (como:log/event logger event)))

(define como:protocol/0.1 0.1)
(define como:protocol/LATEST como:protocol/0.1)

(define (como:event->jsexpr event)
  (hasheq 'source (como:event-source event)
          'type (como:event-type event)
          'protocol-version (como:event-protocol-version event)
          'value (como:event-value event)
          'time (como:event-time event)))