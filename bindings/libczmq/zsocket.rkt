#lang racket/base

(require
 ffi/unsafe
 (only-in racket/vector vector-memq)
 "libczmq.rkt")

(provide
 zsocket/new
 zsocket/destroy
 zsocket/bind
 zsocket/unbind
 zsocket/connect
 zsocket/disconnect
 zsocket/poll
 zsocket/flavor
 zsocket/send)

;; Flags for transmitting zframes
(define ZFRAME/MORE     1) ; More to follow.
(define ZFRAME/DONTWAIT 4) ; Nonblocking transmission of the zframe.

;(define ZMQ_PAIR 0)                                                                                                   
;(define ZMQ_PUB 1)                                                                                                
;(define ZMQ_SUB 2)
;(define ZMQ_REQ 3)                                                                                                       
;(define ZMQ_REP 4)                                                                                                       
;(define ZMQ_DEALER 5)                                                                                                   
;(define ZMQ_ROUTER 6)                                                                                                   
;(define ZMQ_PULL 7)                                                                                                     
;(define ZMQ_PUSH 8)                                                                                                     
;(define ZMQ_XPUB 9)                                                                                                     
;(define ZMQ_XSUB 10)                                                                                                     
;(define ZMQ_STREAM 11)

;; Ordering of zsocket flavors corresponds to ZMQ defines above.
(define FLAVORS
  #(PAIR
    PUB SUB
    REQ REP
    DEALER ROUTER
    PULL PUSH
    XPUB XSUB
    STREAM))

;; Create a new zsocket in the given context.
(define-czmq-function zsocket/raw "zsocket_new" (_fun _zcontext (flavor : _int) -> _zsocket))
(define (zsocket/new context flavor)
  (zsocket/raw context (vector-memq flavor FLAVORS)))
;; Destroy the given zsocket within the given context.
(define-czmq-function zsocket/destroy "zsocket_destroy" (_fun _zcontext _zsocket -> _void))
;; Bind a socket to an endpoint returning the port number if successful.
(define-czmq-function zsocket/bind "zsocket_bind" (_fun _zsocket (endpoint : _string) -> _int))
;; Unbind a zsocket.
(define-czmq-function zsocket/unbind "zsocket_unbind" (_fun _zsocket (endpoint : _string) -> (r : _int = (zero? r))))
;; Connect the zsocket to the given endpoint.
(define-czmq-function zsocket/connect "zsocket_connect" (_fun _zsocket (endpoint : _string) -> (r : _int = (zero? r))))
;; Disconnect the zsocket from the given endpoint.
(define-czmq-function zsocket/disconnect "zsocket_disconnect" (_fun _zsocket (endpoint : _string) -> (r : _int = (zero? r))))
;; Poll the given zsocket waiting at most msecs.
(define-czmq-function zsocket/poll "zsocket_poll" (_fun _zsocket (msecs : _int) -> _bool))
;; Return zsocket flavor (type) as string.
(define-czmq-function zsocket/flavor "zsocket_type_str" (_fun _zsocket -> _string))
;; Send data over a socket as a single zframe.
(define-czmq-function
  zsocket/send/raw "zsocket_sendmem"
  (_fun _zsocket (buffer : _bytes) (size : _int = (bytes-length buffer)) (flags : _int) -> (r : _int = (zero? r))))

(define (zsocket/send socket buffer #:more [more #f] #:wait [wait #f]) ; Default is single frame only with no blocking.
  (zsocket/send/raw socket buffer (bitwise-ior (if more ZFRAME/MORE 0) (if wait 0 ZFRAME/DONTWAIT))))
