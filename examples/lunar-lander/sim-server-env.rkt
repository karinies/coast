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
  "../../uuid.rkt"
  "../examples-env.rkt")

(provide SIM/SERVER/ENV)

;; Still incomplete: time, date, gates.
(define SIM/SERVER/ENV
  (pairs-to-environ
   EXAMPLES/ENVIRON
   (list
    ; display functions
    (define/global/1 'display display)
    (define/global/0 'newline newline)
    
    ; vector functions
    (define/global/N 'vector vector)
    (define/global/2 'vector-ref vector-ref)
    
    ; COAST functions/declarations
    (define/global/1 'duplet/resolver duplet/resolver)
    (define/global/1 'duplet/block duplet/block)
    (define/global/1 'murmur/payload murmur/payload)
    (define/global/N 'islet/curl/new islet/curl/new)
    (define/global/1 'sleep sleep)
    (cons 'GATE/ALWAYS GATE/ALWAYS)
    (define/global/2 'send send)
    
    )))
