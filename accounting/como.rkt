#lang racket/base

(require racket/contract
         json
         "../time.rkt"
         "../promise/base.rkt"
         "../curl/base.rkt")

(provide (contract-out
          [como:log/event/filtered (-> como:logger? como:event? como:filter/c any/c)]
          [como:log (->* (como:logger? como:filter/c #:source string? #:source-islet string? #:type string? #:version number? #:value any/c #:time real? #:place como:place/c #:curl (or/c curl/core? curl?)) void?)]
          [como:event->jsexpr (-> como:event? jsexpr?)]
          [como:transport-messenger/shutdown (-> como:transport-messenger? void?)]
          [struct como:logger ((messenger como:transport-messenger?) (filter como:filter/c))]
          [struct como:event ((source-island string?) (source-islet string?) (type string?) (protocol-version number?) (value jsexpr?) (time real?) (place como:place/c) (curl (or/c curl/core? curl?)))]
          [como:filter/pass? (-> como:filter/c como:event? boolean?)])
         (struct-out como:transport-messenger)
         como:protocol/0.1
         como:protocol/LATEST
         como:filter/TRUE
         como:filter/FALSE
         como:filter/c
         como:place/c)

#|
 | Accountability Logging.
 |#
(struct como:event
  (source-island
   source-islet
   type
   protocol-version
   value
   time
   place
   curl)
  )

(struct como:transport-messenger
  (sender
   stopper)
  #:transparent)

(define (como:transport-messenger/shutdown messenger)
  ((como:transport-messenger-stopper messenger)))

(struct como:logger
  (messenger
   filter)
  #:transparent)

(define como:place/c (or/c place/c #f))

(define como:filter/c (-> como:event? boolean?))

(define como:filter/TRUE (lambda (event) #t))

(define como:filter/FALSE (lambda (event) #f))

(define (como:log/event logger event)
  (como:log/event/filtered logger event como:filter/TRUE))

(define (como:log/event/filtered logger event filter)
  (when (filter event)
    (displayln (format "Logging event ~a using ~a" event (como:logger-messenger logger)))
    (let* ([messenger (como:logger-messenger logger)]
           [sender (como:transport-messenger-sender messenger)])
      (sender event))))

(define (como:log logger filter #:source island #:source-islet islet #:type type #:version version #:value value #:time time #:place place #:curl curl)
  (let ([event (como:event island islet type version value time place curl)])
    (como:log/event/filtered logger event filter)))

(define (como:filter/pass? filter event)
  (filter event))

(define como:protocol/0.1 0.1)
(define como:protocol/LATEST como:protocol/0.1)

(define (como:event->jsexpr event)
  (let ([curl (como:event-curl event)])
  (hasheq 'source-island (como:event-source-island event)
          'source-islet (como:event-source-islet event)
          'type (como:event-type event)
          'version (como:event-protocol-version event)
          'value (como:event-value event)
          'time (como:event-time event)
          'place (let ([place (como:event-place event)])
                   (if (symbol? place) (symbol->string place) place))
          'curl-id (symbol->string (if (curl? curl) (curl/id curl) (curl/core-id curl))))))