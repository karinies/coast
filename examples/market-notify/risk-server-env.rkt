#lang racket/base

(require
  "../../baseline.rkt"
  "../../persistent/environ.rkt"
  "./risk-server-register.rkt"
  "../../promise.rkt"
  "../../promise/base.rkt"
  "../../murmur.rkt"
  "../../send.rkt"
  "../../transport/gate.rkt"
  "../../transport/access.rkt"
  "../../islet.rkt"
  "../../curl/base.rkt"
  "../../spawn.rkt")

(provide
 RISK/SERVER/ENV)

;; Still incomplete: time, date, gates.
(define RISK/SERVER/ENV
  (pairs-to-environ
   BASELINE
   (list
    ; Hack for now.
    (define/global/1 'display display) ; HACK, HACK, HACK.
    (define/global/2 'register risk/subscribe)
    (define/global/0 'risk/subs/count risk/subs/count)
    (define/global/1 'duplet/resolver duplet/resolver)
    (define/global/1 'duplet/block duplet/block)
    (define/global/1 'murmur/payload murmur/payload)
    (define/global/N 'islet/curl/new islet/curl/new)
    (define/global/0 'this/islet/nickname this/islet/nickname)
    (cons 'GATE/ALWAYS GATE/ALWAYS)
    (cons 'INTRA INTRA)
    (define/global/2 'send send)
    )))