#lang racket/base

(require
	"../baseline.rkt"
	"../persistent/environ.rkt"
	"market-server-register.rkt"
        "../promise.rkt"
        "../spawn.rkt")

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
    (define/global/2 'spawn/exchange/duplet* spawn/exchange/duplet*)
    (define/global/1 'duplet/resolver duplet/resolver)
    )))
