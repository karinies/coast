#lang racket/base

(require racket/contract
         json)

(provide (contract-out
          [como:log (-> como:logger? como:event? any/c)]
          [como:event->jsexpr (-> como:event? jsexpr?)])
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
  (sender)
  #:transparent)

(struct como:logger
  (messenger)
  #:transparent)

(define (como:log logger event)
  (let* ([messenger (como:logger-messenger logger)]
         [sender (como:transport-messenger-sender messenger)])
    (sender event)))

(define como:protocol/0.1 0.1)
(define como:protocol/LATEST como:protocol/0.1)

(define (como:event->jsexpr event)
  (hasheq 'source (como:event-source event)
          'type (como:event-type event)
          'protocol-version (como:event-protocol-version event)
          'value (como:event-value event)
          'time (como:event-time event)))