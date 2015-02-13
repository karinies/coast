#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 ; Set options.
 zsocket/identity/set
 zsocket/ipv6/set
 zsocket/immediate/set
 zsocket/router/mandatory/set
 zsocket/request/relaxed/set
 zsocket/conflate/set
 zsocket/subscribe
 zsocket/unsubscribe
 ;zsocket/curve/public/set

 ; Get options.
 zsocket/ipv6?
 ;zsocket/curve?
 ;zsocket/flavor
 zsocket/send/max
 zsocket/receive/max
 zsocket/identity
 zsocket/receive/timeout
 zsocket/send/timeout
 zsocket/events
 zsocket/receive/more?
 zsocket/fd)

;; Given an ASCII byte string or an ASCII string return an ASCII string.
(define (bytes=>string x)
  (if (bytes? x) (bytes->string/utf-8 x) x))

;; Get options.
;(define-czmq-function zsocket/flavor "zsocket_type" (_fun _zsocket -> _int))
(define-czmq-function zsocket/send/max "zsocket_sndhwm" (_fun _zsocket -> _int))
(define-czmq-function zsocket/receive/max "zsocket_rcvhwm" (_fun _zsocket -> _int))
(define-czmq-function zsocket/identity "zsocket_identity" (_fun _zsocket -> _string))
;; -1 = blocking, 0 = nonblocking, m > 0 = blocking for at most m milliseconds.
(define-czmq-function zsocket/receive/timeout "zsocket_rcvtimeo" (_fun _zsocket -> _int))
(define-czmq-function zsocket/send/timeout "zsocket_sndtimeo" (_fun _zsocket -> _int))
(define-czmq-function zsocket/events "zsocket_events" (_fun _zsocket -> _int))
(define-czmq-function zsocket/receive/more? "zsocket_rcvmore" (_fun _zsocket -> _bool))
;; Return the raw Unix file descriptor of a zsocket.
(define-czmq-function zsocket/fd "zsocket_fd" (_fun _zsocket -> _int))

;; Set options.
(define-czmq-function
  zsocket-identity-set
  "zsocket_set_identity"
  (_fun _zsocket (id : _string) -> _void))
(define (zsocket/identity/set socket s)
  (zsocket-identity-set socket (bytes=>string s)))

;; -1 = blocking, 0 = nonblocking, m > 0 = blocking for at most m milliseconds.
(define-czmq-function zsocket/receive/timeout/set "zsocket_set_rcvtimeo" (_fun _zsocket _int -> _void))

;; If flag is #t permit both IPv4 and IPv6. If flag is #f then IPv4 only.
(define-czmq-function zsocket/ipv6/set "zsocket_set_ipv6" (_fun _zsocket (flag : _bool) -> _void))
(define-czmq-function zsocket/ipv6? "zsocket_ipv6"     (_fun _zsocket -> (r : _bool)))
;(define zsocket/option/ipv6
;  (case-lambda
;    ((socket)      (zsocket/ipv6/get socket))
;    ((socket flag) (zsocket/ipv6/set socket flag))))

;; If fiag is #t queue messages only to completed outgoing connections. Default is #f.
(define-czmq-function zsocket/immediate/set "zsocket_set_immediate" (_fun _zsocket (flag : _bool) -> _void))

;; If flag is #f unroutable messages are silently discarded. If #t an unroutable message raises EHOSTUNREACH.
(define-czmq-function zsocket/router/mandatory/set "zsocket_set_router_mandatory" (_fun _zsocket (flag : _bool) -> _void))

;; If #t then socket automatically generates an empty message whenever a connection is made or accepted.
;; If #f then no connection events are generated.
;; Default is #f.
;(define-czmq-function zsocket/router/probe "zsocket_probe_router" (_fun _zsocket -> _bool))

;; If flag is #t relaxe the strict correspondence between requests and responses on a request socket.
;; If flag #f maintain the strict correspondence between requests and responses on a request socket.
(define-czmq-function zsocket/request/relaxed/set "zsocket_set_req_relaxed" (_fun _zsocket (flag : _bool) -> _void))

;; If flag is #t match request/reply pairs via a request id.
;; If flag is #f match request/reply pairs in time where a reply is always paired with the most recent request.
(define-czmq-function zsocket/request/correlate/set "zsocket_set_req_correlate" (_fun _zsocket (flag : _bool) -> _void))

;; If flag is #t the socket will keep only the most recent message in its queue.
;; If flag is #f the socket will retain sent/received messages upto the socket high-water mark.
(define-czmq-function zsocket/conflate/set "zsocket_set_conflate" (_fun _zsocket (flag : _bool) -> _void))

(define-czmq-function zsocket/subscribe "zsocket_set_subscribe" (_fun _zsocket (filter : _string) -> _void))
(define-czmq-function zsocket/unsubscribe "zsocket_set_unsubscribe" (_fun _zsocket (filter : _string) -> _void))

#| Hold off implementing this stuff for now. I doubt that I have it quite right at this point.
;; If flag is #t socket will enforce CurveCP cryptographic connections.
;; If flag is #f then socket will not enforce any CurveCP security.
(define-czmq-function zsocket/curve/set "zsocket_set_curve_server" (_fun _zsocket (flag : _bool) -> _void))
;; Return #t if given socket is CurveCP enabled and #f otherwise.
(define-czmq-function zsocket/curve? "zsocket_curve_server" (_fun _zsocket -> _bool))
;; Set the CurveCP public key for the given socket.
(define-czmq-function zsocket/curve/public/z85/set "zsocket_set_curve_publickey" (_fun _zsocket (key : _string) -> _void))

(define-czmq-function zsocket/curve/public/binary/set "zsocket_set_curve_publickey_bin" (_fun _zsocket (key : _bytes) -> _void))
(define (zsocket/curve/public/set socket key)
  (cond
    ((string? key) (zsocket/curve/public/z85/set socket key)) ; Key is string in z85 encoding.
    ((bytes? key)
     (if (= (bytes-length key) 32)
         (zsocket/curve/public/binary/set socket key) ; Binary public key as byte string.
         (zsocket/curve/public/z85/set socket (bytes->string/latin-1 key)))))) ; Byte string in z85 encoding.
|#
