#lang racket/base

(require
 "../curve.rkt"
 ;"../curl/base.rkt"
 "../curl/curl.rkt"
 "../persistent/environ.rkt"
 "../serialize.rkt"
 "../this.rkt"
 "../transport/access.rkt"
 "../transport/transport-bankers.rkt"
 "../uuid.rkt")

(define t (transport:bankers/new))
(define a (access:send/new t 'access:send-test GATES/NONE EMBARGO/NO))
; (struct curl/core (id origin path access/id created metadata) #:transparent)
(define core1 (curl/core/new (curve/kp/base64 CURVE/STATIC) '(a b c) a environ/null))
;(define core2 (curl/core/new (curve/kp/base64 CURVE/STATIC) '(a b c) a #f))
;(define u1 (curl/new core1 (curve/kp/sign CURVE/STATIC) (curve/ks/sign CURVE/STATIC)))
;(define u2 (curl/new core2 (curve/kp/sign CURVE/STATIC) (curve/ks/sign CURVE/STATIC)))
;(define z1 (curl/core-to-curl/zpl/signed core1 (curve/ks/sign CURVE/STATIC)))
;(define z2 (curl/core-to-curl/zpl/signed core2 (curve/ks/sign CURVE/STATIC)))
;(define z1/safe (curl/zpl/signed-to-curl/zpl/safe z1))
;(define z2/safe (curl/zpl/signed-to-curl/zpl/safe z2))






;(define z (curl/core/zpl (curl/body u) (curl/serial/z85 u)))
;(define verify (curl/verify? u (curve/kp/sign keys)))
