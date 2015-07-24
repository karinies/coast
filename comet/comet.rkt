#lang racket/base

(require racket/contract/base
         racket/bool
         json
         "../time.rkt"
         "../promise/base.rkt"
         "../curl/base.rkt")

(provide (contract-out
          [comet:log/event/filtered (-> comet:logger? comet:event? comet:filter/c any/c)]
          [comet:log (->* (comet:logger? comet:filter/c #:source string? #:source-islet (or/c string? #f) #:type string? #:version number? #:value any/c #:time real? #:place comet:place/c #:curl comet:curl/c) void?)]
          [comet:event->jsexpr (-> comet:event? jsexpr?)]
          [comet:transport-messenger/shutdown (-> comet:transport-messenger? void?)]
          [struct comet:logger ((messenger comet:transport-messenger?) (filter comet:filter/c))]
          [struct comet:event ((source-island string?) (source-islet string?) (type string?) (protocol-version number?) (value jsexpr?) (time real?) (place comet:place/c) (curl comet:curl/c))]
          [comet:filter/pass? (-> comet:filter/c comet:event? boolean?)])
         (struct-out comet:transport-messenger)
         comet:protocol/0.1
         comet:protocol/LATEST
         comet:filter/TRUE
         comet:filter/FALSE
         comet:filter/c
         comet:place/c
         comet:curl/c)

#|
 | Accountability Logging.
 |#
(struct comet:event
  (source-island
   source-islet
   type
   protocol-version
   value
   time
   place
   curl)
  )

(struct comet:transport-messenger
  (sender
   stopper)
  #:transparent)

(define (comet:transport-messenger/shutdown messenger)
  ((comet:transport-messenger-stopper messenger)))

(struct comet:logger
  (messenger
   filter)
  #:transparent)

(define comet:curl/c (or/c curl/core? curl? #f))

(define comet:place/c (or/c place/c #f))

(define comet:filter/c (-> comet:event? boolean?))

(define comet:filter/TRUE (lambda (event) #t))

(define comet:filter/FALSE (lambda (event) #f))

(define (comet:log/event logger event)
  (comet:log/event/filtered logger event comet:filter/TRUE))

(define (comet:log/event/filtered logger event filter)
  (when (filter event)
    ;(displayln (format "Logging event ~a using ~a" event (comet:logger-messenger logger)))
    (let* ([messenger (comet:logger-messenger logger)]
           [sender (comet:transport-messenger-sender messenger)])
      (sender event))))

(define (comet:log logger filter #:source island #:source-islet islet #:type type #:version version #:value value #:time time #:place place #:curl curl)
  (let ([event (comet:event island islet type version value time place curl)])
    (comet:log/event/filtered logger event filter)))

(define (comet:filter/pass? filter event)
  (filter event))

(define comet:protocol/0.1 0.1)
(define comet:protocol/LATEST comet:protocol/0.1)

(define (comet:event->jsexpr event)
  (let ([curl (comet:event-curl event)])
  (hasheq 'source-island (comet:event-source-island event)
          'source-islet (comet:event-source-islet event)
          'type (comet:event-type event)
          'version (comet:event-protocol-version event)
          'value (comet:event-value event)
          'time (comet:event-time event)
          'place (let ([place (comet:event-place event)])
                   (if (symbol? place) (string-downcase (symbol->string place)) place))
          'curl-id (if (false? curl) #f (symbol->string (if (curl? curl) (curl/id curl) (curl/core-id curl)))))))