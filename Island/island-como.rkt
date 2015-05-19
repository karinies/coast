#lang racket

(require
  "../time.rkt"
  (only-in "../islet.rkt" this/island/nickname this/islet/nickname)
  "../accounting/como.rkt"
  "../promise/base.rkt")

(provide
 (contract-out
  [island/monitoring/start (-> symbol? como:transport-messenger? void?)]
  [island/monitoring/shutdown (-> symbol? void?)]
  [island/monitoring/log (->* (#:type string? #:place como:place/c) void?)]
  [island/monitoring/set/filter (-> symbol? como:filter/c void?)]
  [island/monitoring/pass? (->* (#:type string? #:place como:place/c) boolean?)]))

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

(define (filter/get nickname)
  (hash-ref filters nickname como:filter/FALSE)) ; If filter hasn't been set, assume #f.

(define (island/monitoring/log #:type type #:place place)
  (let ([logger (hash-ref loggers (this/island/nickname) #f)]
        [filter (filter/get (this/island/nickname))])
    (when (como:logger? logger)
      (como:log logger filter #:source (symbol->string (this/island/nickname))
                #:source-islet (symbol->string (this/islet/nickname)) 
                #:type type 
                #:version como:protocol/LATEST 
                #:value #f 
                #:time (time/now/milliseconds)
                #:place place))))

(define (island/monitoring/pass? #:type type #:place place)
  (let ([event (como:event (symbol->string (this/island/nickname)) (symbol->string (this/islet/nickname)) type como:protocol/LATEST #f (time/now/milliseconds) place)])
    (como:filter/pass? (filter/get (this/island/nickname)) event)))