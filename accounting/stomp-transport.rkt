#lang racket/base

(require racket/contract
         (planet tonyg/stomp:3:2)
         json
         "como.rkt")

(provide
 (contract-out 
  [stomp-messenger-new (->* (#:host string? #:login string? #:pass string? #:destination string?) (#:vhost string?) stomp-messenger?)])
 stomp-messenger
 )

#|
 | STOMP Transport Messaging.
 |#

(struct stomp-messenger
  (session) #:super struct:como:transport-messenger #:transparent)

(define (stomp-messenger-new #:host host #:login login #:pass pass #:destination destination #:vhost [vhost "/"])
  (let* ([s (stomp-connect host
                           #:login login
                           #:passcode pass
                           #:virtual-host vhost)]
         [sender (lambda (payload) 
                   (stomp-send s destination (jsexpr->bytes (como:event->jsexpr payload)))
                   (stomp-flush s))]
         [stopper (lambda () (stomp-disconnect s))]
         [messenger (stomp-messenger sender stopper s)])
    messenger))