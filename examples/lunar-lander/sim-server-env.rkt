#lang racket/base

(require racket/vector)
(require
  "../../baseline.rkt"
  "../../persistent/environ.rkt"
  "../../promise.rkt"
  "../../murmur.rkt"
  "../../send.rkt"
  "../../transport/gate.rkt"
  "../../islet.rkt"
  "../../islet-utils.rkt"
  "../../uuid.rkt"
  "../examples-env.rkt")

(provide SIM/SERVER/ENV)

;; Still incomplete: time, date, gates.
(define SIM/SERVER/ENV
  (pairs-to-environ
   EXAMPLES/ENVIRON
   (list
    ; Hack for now.
    (define/global/1 'display display) ; HACK, HACK, HACK.
    (define/global/0 'newline newline)
    
    ; vector functions
    (define/global/1 'vector? vector?)
    (define/global/N 'vector vector)
    (define/global/2 'vector-ref vector-ref)
    (define/global/3 'vector-set! vector-set!)
    (define/global/1 'struct->vector struct->vector)
    (define/global/N 'vector-append vector-append)
    
    (define/global/1 'duplet/resolver duplet/resolver)
    (define/global/1 'duplet/block duplet/block)
    (define/global/1 'murmur/payload murmur/payload)
    (define/global/N 'islet/curl/new islet/curl/new)
    (define/global/0 'this/islet/nickname this/islet/nickname)
    (define/global/N 'motile/call motile/call)
    (define/global/3 'subislet/callback/new subislet/callback/new)
    (define/global/0 'uuid/symbol uuid/symbol)
    (define/global/1 'sleep sleep)
    (cons 'BASELINE/SPAWN BASELINE/SPAWN)
    (cons 'GATE/ALWAYS GATE/ALWAYS)
    (cons 'INTRA INTRA)
    (cons 'EXAMPLES/ENVIRON EXAMPLES/ENVIRON)
    (define/global/2 'send send)
    
    )))