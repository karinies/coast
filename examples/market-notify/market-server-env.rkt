#lang racket/base

(require
  "../../baseline.rkt"
  "../../persistent/environ.rkt"
  "./market-server-register.rkt"
  "../../promise.rkt"
  "../../murmur.rkt"
  "../../send.rkt"
  "../../transport/gate.rkt"
  "../../transport/access.rkt"
  "../../islet.rkt"
  "../../curl/base.rkt"
  "../../spawn.rkt")

(provide
 MARKET/SERVER/ENV)

;; Still incomplete: time, date, gates.
(define MARKET/SERVER/ENV
  (pairs-to-environ
   BASELINE
   (list
    ; Hack for now.
    (define/global/1 'display display) ; HACK, HACK, HACK.
    (define/global/2 'register market/subscribe)
    (define/global/0 'market/subs/count market/subs/count)
    (define/global/1 'duplet/resolver duplet/resolver)
    (define/global/1 'duplet/block duplet/block)
    (define/global/1 'murmur/payload murmur/payload)
    (define/global/N 'islet/curl/new islet/curl/new)
    (define/global/0 'this/islet/nickname this/islet/nickname)
    (cons 'GATE/ALWAYS GATE/ALWAYS)
    (cons 'INTRA INTRA)
    (define/global/2 'send send)
    )))