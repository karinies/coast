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
  )

(provide SIMULATOR/ENVIRON)

(define SIMULATOR/ENVIRON
  (pairs-to-environ
   BASELINE/SPAWN 
   (list
    (define/global/0 'newline newline)
    (define/global/1 'duplet/resolver duplet/resolver)
    (define/global/1 'duplet/block duplet/block)
    (define/global/1 'murmur/payload murmur/payload)
    (define/global/N 'islet/curl/new islet/curl/new)
    (define/global/N 'motile/call motile/call)
    (define/global/1 'sleep sleep)
    (cons 'GATE/ALWAYS GATE/ALWAYS)
    (define/global/2 'send send)
    
    )))