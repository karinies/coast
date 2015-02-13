#lang racket/base

(require ffi/unsafe)

(provide
; czmq
 define-czmq-constant
 define-czmq-function
 ; C pointer types.
 _zauth
 _zbeacon
 _zcert
 _zcertstore
 _zconfig
 _zconfig/null
 _zcontext
 _zframe
 _zhash
 _zhash/null
 _zlist
 _zlist/null
 _zmessage
 _zpoller
 _zpoller
 _zsocket
 _zsocket/null
 _size_t
 ; Racket-level tests for C pointer types.
 zbeacon?
 zconfig?
 zcontext?
 zframe?
 zhash?
 zlist?
 zmessage?
 zsocket?)

(define-cpointer-type _zauth)
(define-cpointer-type _zbeacon)
(define-cpointer-type _zcert)
(define-cpointer-type _zcertstore)
(define-cpointer-type _zconfig)
(define-cpointer-type _zcontext)
(define-cpointer-type _zframe)
(define-cpointer-type _zhash)
(define-cpointer-type _zlist)
(define-cpointer-type _zmessage)
(define-cpointer-type _zpoller)
(define-cpointer-type _zsocket)
(define _size_t _uint)

;; Find the shared library libczmq.
(define czmq (ffi-lib "libczmq"))

;; czmq constants.
(define-syntax-rule (define-czmq-constant binding name type) ; type is usually _int or _bytes
  (define binding (get-ffi-obj name czmq (_fun -> type))))

;; Shorthand for binding czmq functions.
(define-syntax-rule (define-czmq-function binding name type)
  (define binding (get-ffi-obj name czmq type)))


