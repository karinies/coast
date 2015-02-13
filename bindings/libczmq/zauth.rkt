#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(define-czmq-function zauth/new "zauth_new" (_fun _zcontext -> _zauth))
(define-czmq-function zauth/destroy "zauth_destroy" (_fun (_ptr i _zauth) -> _void))
;; Add an IP address to the whitelist.
(define-czmq-function zauth/allow "zauth_allow" (_fun _zauth (address : _string) -> _void))
;; Add an IP address to the blacklist.
;; If both a whitelist and a blacklist are defined then only the whitelist applies.
(define-czmq-function zauth/deny "zauth_deny" (_fun _zauth (address : _string) -> _void))
;; Configure CURVE-based authentication. location is the directory containing the relevant public key certificates (zcerts).
;; To permit any domain use "*" and to allow all client public keys without checking specify "*" for the location. 
(define-czmq-function zauth/curve/configure "zauth_configure_curve" (_fun _zauth (domain : _string) (location : _string) -> _void))
;; For turning tracing on and off. 
(define-czmq-function zauth/verbose/set "zauth_set_verbose" (_fun _zauth (trace? : _bool) -> _void))