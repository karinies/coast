#lang racket/base

(require
  racket/bool
  racket/string
  racket/contract/base
  "../time.rkt"
  "../curl/base.rkt"
  (only-in "../islet.rkt" this/island/nickname this/islet/nickname)
  "../comet/comet.rkt"
  "../comet/comet-types.rkt"
  "../promise/base.rkt")

(provide
 (contract-out
  [island/monitoring/start (-> symbol? comet:transport-messenger? void?)]
  [island/monitoring/shutdown (-> symbol? void?)]
  [island/monitoring/log (->* (#:type string? #:place comet:place/c #:curl comet:curl/c) (#:value comet:value/c) void?)]
  [island/monitoring/set/filter (-> symbol? comet:filter/c void?)]
  [island/monitoring/pass? (->* (#:type string? #:place comet:place/c #:curl comet:curl/c) boolean?)]))

;; Coast Monitoring stuff.
(define loggers (make-hash)) ; Loggers use to log events. (-> symbol? comet:logger?)
(define filters (make-hash)) ; Filters for loggers. (-> symbol? comet:filter/c)

(define (island/monitoring/start island messenger)
  (let ([filter comet:filter/TRUE])
    (hash-set! loggers island (comet:logger messenger filter))
    (island/monitoring/set/filter island filter))
  (island/monitoring/log/start* #:source island))

(define (island/monitoring/set/filter what filter)
  (hash-set! filters what filter))

;(define (island/monitoring/pause what)
;  (let ([logger (hash-ref loggers what #f)])
;    (when (comet:logger? logger)
;      (set-comet:logger-filter! what comet:filter/FALSE)))) ; TO-DO Store previous value somewhere for future unpausing...

(define (island/monitoring/shutdown what)
  (let ([logger (hash-ref loggers what #f)])
    (when (comet:logger? logger)
      (let ([messenger (comet:logger-messenger logger)])
        (comet:transport-messenger/shutdown messenger)))))

(define (filter/get nickname)
  (hash-ref filters nickname comet:filter/FALSE)) ; If filter hasn't been set, assume #f.

(define (island/monitoring/log/start* #:source island)
  (island/log* #:source island
               #:type COMET/ISLAND/START
               #:source-islet #f
               #:version comet:protocol/LATEST
               #:time (time/now/milliseconds)
               #:place #f
               #:curl #f
               #:value #f))
         
(define (island/log* #:source source #:source-islet islet #:type type #:version version #:time time #:place place #:curl curl #:value value)
  (let ([logger (hash-ref loggers source #f)]
        [filter (filter/get source)])
    (when (comet:logger? logger)
      (comet:log logger filter #:source (symbol->string source)
                #:source-islet (if (false? islet) #f (car (string-split (symbol->string islet) "@")))
                ;#:source-islet (if (false? islet) #f (symbol->string islet))
                #:type type 
                #:version version 
                #:value value
                #:time time
                #:place place
                #:curl curl))))

(define (island/monitoring/log #:type type #:place place #:curl curl #:value [value #f])
  (island/log* #:source (this/island/nickname)
               #:source-islet (this/islet/nickname)
               #:type type
               #:version comet:protocol/LATEST 
               #:value value
               #:time (time/now/milliseconds)
               #:place place
               #:curl curl))

(define (island/monitoring/pass? #:type type #:place place #:curl curl)
  (let ([event (comet:event (symbol->string (this/island/nickname)) (symbol->string (this/islet/nickname)) type comet:protocol/LATEST #f (time/now/milliseconds) place curl)])
    (comet:filter/pass? (filter/get (this/island/nickname)) event)))