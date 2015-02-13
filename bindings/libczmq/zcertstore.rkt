#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 zcertstore/new
 zcertstore/destroy
 zcertstore/lookup
 zcertstore/insert)

(define-czmq-function zcertstore/new "zcertstore_new" (_fun (path : _string) -> _zcertstore))
(define-czmq-function zcertstore/destroy "zcertstore_destroy" (_fun (_ptr i _zcertstore) -> _void))
(define-czmq-function zcertstore/lookup "zcertstore_lookup" (_fun _zcertstore (public : _string) -> _zcert))
(define-czmq-function zcertstore/insert "zcertstore_insert" (_fun _zcertstore (_ptr i _zcert) -> _void))
