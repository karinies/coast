#lang racket

(require
  "../time.rkt"
  "../islet.rkt"
  "../accounting/como.rkt"
  "../Island/base.rkt")

(provide
 (contract-out
  [island/monitoring/start (-> symbol? como:transport-messenger? void?)]
  [island/monitoring/shutdown (-> symbol? void?)]
  [island/monitoring/log (->* (#:type string? #:value any/c) void?)]
  [island/monitoring/set/filter (-> symbol? como:filter/c void?)]))

;; Coast Monitoring stuff.
(define loggers (make-hash)) ; Loggers use to log events. (-> symbol? como:logger?)
(define filters (make-hash)) ; Filters for loggers. (-> symbol? como:filter/c)

(define (island/monitoring/start what messenger)
  (let ([filter como:filter/TRUE])
    (hash-set! loggers what (como:logger messenger filter))
    (island/monitoring/set/filter what filter)))

(define (island/monitoring/set/filter what filter)
  (hash-set! filters what filter))

;(define (island/monitoring/pause what)
;  (let ([logger (hash-ref loggers what #f)])
;    (when (como:logger? logger)
;      (set-como:logger-filter! what como:filter/FALSE)))) ; TO-DO Store previous value somewhere for future unpausing...

(define (island/monitoring/shutdown what)
  (let ([logger (hash-ref loggers what #f)])
    (when (como:logger? logger)
      (let ([messenger (como:logger-messenger logger)])
        (como:transport-messenger/shutdown messenger)))))

(define (island/monitoring/log #:type type #:value value)
  (let ([logger (hash-ref loggers (this/island/nickname) #f)]
        [filter (hash-ref filters (this/island/nickname) como:filter/FALSE)]) ; If filter hasn't been set, assume #f.
    (when (como:logger? logger)
      (como:log logger filter #:source (symbol->string (this/island/nickname))
                #:source-islet (symbol->string (this/islet/nickname)) 
                #:type type 
                #:version como:protocol/LATEST 
                #:value value 
                #:time (time/now/milliseconds)))))

(define (island/monitoring/pass? #:type type #:value value)
  (let ([event (como:event (symbol->string (this/island/nickname)) (symbol->string (this/islet/nickname)) type como:protocol/LATEST value (time/now/milliseconds))])
    (como:filter/pass? event)))