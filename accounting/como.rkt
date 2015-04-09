#lang racket/base

(require racket/contract
         json)

(provide (contract-out
          [como:log (-> como:logger? como:event? void?)]
          [como:event->jsexpr (-> como:event? jsexpr?)])
         como:event
         como:logger
         struct:como:transport-messenger
         como:protocol/VERSION)

#|
 | Accountability Logging.
 |#
(struct como:event
  (source
   type
   protocol-version
   value)
  )

(struct como:transport-messenger
  (sender))

(struct como:logger
  (messenger))

(define (como:log logger event)
  (como:transport-messenger-sender (como:logger-messenger logger)) event)

(define como:protocol/VERSION 0.1)

(define (como:event->jsexpr event)
  (hasheq 'source (como:event-source event)
          'type (como:event-type event)
          'protocol-version (como:event-protocol-version event)
          'value (como:event-value event)))