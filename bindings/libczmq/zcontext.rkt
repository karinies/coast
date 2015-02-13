#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 zcontext/new
 zcontext/destroy
 zcontext/threads
 zcontext/linger
 zcontext/pipe/peak
 zcontext/send/peak
 zcontext/receive/peak)

;; Create a new context.
(define-czmq-function zcontext/new "zctx_new" (_fun -> _zcontext))
;; Destroy the context and all of the sockets that it contains.
(define-czmq-function zcontext/destroy "zctx_destroy" (_fun (_ptr i _zcontext) -> _void))
;; Raise the number of i/o threads from 1. Must be set before any zsockets are created.
(define-czmq-function zcontext/threads "zctx_set_iothreads" (_fun _zcontext _int -> _void))
;; Set slack (in msecs) to flush sockets before closing them. Default is 0.
;; Under default all in-transit messages are lost when a socket or context is closed.
(define-czmq-function zcontext/linger "zctx_set_linger" (_fun _zcontext _int -> _void))
;; Set message high-water mark for pipe zsockets. Default is 1000.
(define-czmq-function zcontext/pipe/peak "zctx_set_pipehwm" (_fun _zcontext _int -> _void))
;; Set outgoing message high-water mark for all other forms of zsocket. Default is 1000.
(define-czmq-function zcontext/send/peak "zctx_set_sndhwm" (_fun _zcontext _int -> _void))
;; Set incoming message high-water mark for all other forms of zsocket. Default is 1000.
(define-czmq-function zcontext/receive/peak "zctx_set_rcvhwm" (_fun _zcontext _int -> _void))