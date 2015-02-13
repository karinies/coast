#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 zcert/any/new
 zcert/given/new
 zcert/destroy
 zcert/key/public
 zcert/key/secret
 zcert/meta/set
 zcert/meta/get
 zcert/meta/keys
 zcert/load
 zcert/save)

(define-czmq-function zcert/any/new "zcert_new" (_fun -> _zcert))
(define-czmq-function zcert/given/new "zcert_new_from" (_fun (public : _bytes) (secret : _bytes) -> _zcert))
(define-czmq-function zcert/destroy "zcert_destroy" (_fun (_ptr i _zcert) -> _void))

(define-czmq-function zcert/key/public "zcert_public_key" (_fun _zcert -> (_bytes o 32)))
(define-czmq-function zcert/key/secret "zcert_secret_key" (_fun _zcert -> (_bytes o 32)))

(define-czmq-function zcert/meta/set "zcert_set_meta" (_fun _zcert (key : _string) (value : _string) -> _void))
(define-czmq-function zcert/meta/get "zcert_meta" (_fun _zcert (key : _string) -> (value : _string)))
(define-czmq-function zcert/meta/keys "zcert_meta_keys" (_fun _zcert -> _zlist))

;; path is used as prefix in <path>_secret for secret certificate and failing that as <path> for public certificate.
(define-czmq-function zcert/load "zcert_load" (_fun (path : _string) -> _zcert))
(define-czmq-function zcert/save "zcert_save" (_fun _zcert (path : _string) -> _bool))

;(define c (zcert/any/new))
;(zcert/save c "bob")

