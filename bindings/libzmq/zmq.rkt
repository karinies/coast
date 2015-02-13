#lang racket/base

(require
 ffi/unsafe
 "libzmq.rkt")

(provide
 zmq/assert/0
 zmq/assert/0...
 zmq/socket/bind
 zmq/connect
 zmq/context/close
 zmq/context/new
 (rename-out (zmq-context? zmq/context?))
 zmq/error
 zmq/error/string
 zmq/socket/close
 zmq/socket/new
 zmq/receive
 zmq/send
 zmq_z85_encode
 zmq_z85_decode
 (rename-out (zmq-socket? zmq/socket?))
 
 ; Send/receive options.
 ZMQ/NO-WAIT
 ZMQ/SEND-MORE
 ; Socket event flags.
 ZMQ/POLLIN
 ZMQ/POLLOUT
 ZMQ/POLLERR
)

(define-cpointer-type _zmq-context)
(define-cpointer-type _zmq-socket)
(define _SIZE_T _ulong)

;; Raise an exception if the C return value r was nonzero
;; giving the routine name and a brief descriptive message.
(define (zmq/assert/0 who r)
  (unless (zero? r)
    (error who "failed: ~a" (zmq/error/string r))))

;; Raise an exception if the C return value r was -1
;; giving the routine name and a brief descriptive message.
(define (zmq/assert/0... who r)
  (if (negative? r)
    (error who "failed: ~a" (zmq/error/string (zmq/error)))
    r))

(define-zmq-function
  zmq/socket/bind "zmq_bind"
  (_fun _zmq-socket _string -> (r : _int) -> (zmq/assert/0 'zmq/bind r)))

(define-zmq-function zmq/context/new "zmq_ctx_new" (_fun -> _zmq-context))

(define-zmq-function zmq/error "zmq_errno" (_fun -> _int))

(define-zmq-function zmq/error/string "zmq_strerror" (_fun _int -> _string))

(define-zmq-function
  zmq/receive "zmq_recv"
  (_fun _zmq-socket (b : _bytes) (_SIZE_T = (bytes-length b)) (_int = 0) -> (r : _int) -> (zmq/assert/0... 'zmq/receive r)))
;(define (zmq/socket/receive socket buffer) (zmq_receive socket buffer (bytes-length buffer) 0))

(define-zmq-function
  zmq/send "zmq_send"
  (_fun _zmq-socket (b : _bytes) (_SIZE_T = (bytes-length b)) (_int = 0) -> (r : _int) -> (zmq/assert/0... 'zmq/receive r)))
;(define (zmq/socket/send socket buffer) (zmq_send socket buffer (bytes-length buffer) 0))

(define-zmq-function zmq_socket "zmq_socket" (_fun _zmq-context _int -> _zmq-socket))
(define (zmq/socket/new context flavor)
  (zmq_socket
   context
   (case flavor
     [(PAIR)      ZMQ/PAIR]
     [(PUBLISH)   ZMQ/PUB]
     [(SUBSCRIBE) ZMQ/SUB]
     [(REQUEST)   ZMQ/REQ]
     [(REPLY)     ZMQ/REP]
     [(DEALER)    ZMQ/DEALER]
     [(ROUTER)    ZMQ/ROUTER]
     [(PULL)      ZMQ/PULL]
     [(PUSH)      ZMQ/PUSH]
     [(XPUB)      ZMQ/XPUB]
     [(XSUB)      ZMQ/XSUB]
     [(STREAM)    ZMQ/STREAM])))

(define-zmq-function
  zmq/socket/close "zmq_close"
  (_fun _zmq-socket -> (r : _int) -> (zmq/assert/0 'zmq/socket/close r)))

(define-zmq-function
  zmq/context/close "zmq_ctx_destroy"
  (_fun _zmq-context -> (r : _int) -> (zmq/assert/0 'zmq/context/close r)))

(define-zmq-function
  zmq/connect "zmq_connect"
  (_fun _zmq-socket _string -> (r : _int) -> (zmq/assert/0 'zmq/connect r)))

(define-zmq-function zmq_z85_encode "zmq_z85_encode" (_fun _string*/latin-1 _bytes _SIZE_T -> _string*/latin-1))
;; uint8_t *zmq_z85_decode (uint8_t *dest, char *string)
(define-zmq-function zmq_z85_decode "zmq_z85_decode" (_fun _bytes _string*/latin-1 -> _void))

#| Taken from zmq.h version 4.0.1
#define ZMQ_PAIR 0
#define ZMQ_PUB 1
#define ZMQ_SUB 2
#define ZMQ_REQ 3
#define ZMQ_REP 4
#define ZMQ_DEALER 5
#define ZMQ_ROUTER 6
#define ZMQ_PULL 7
#define ZMQ_PUSH 8
#define ZMQ_XPUB 9
#define ZMQ_XSUB 10
#define ZMQ_STREAM 11
|#

;; Socket types for zmq/socket.
(define ZMQ/PAIR    0)
(define ZMQ/PUB     1)
(define ZMQ/SUB     2)
(define ZMQ/REQ     3)
(define ZMQ/REP     4)
(define ZMQ/DEALER  5)
(define ZMQ/ROUTER  6)
(define ZMQ/PULL    7)
(define ZMQ/PUSH    8)
(define ZMQ/XPUB    9)
(define ZMQ/XSUB   10)
(define ZMQ/STREAM 11)

;; Taken from zmq.h version 4.0.1
#|
#define ZMQ_AFFINITY 4
#define ZMQ_IDENTITY 5
#define ZMQ_SUBSCRIBE 6
#define ZMQ_UNSUBSCRIBE 7
#define ZMQ_RATE 8
#define ZMQ_RECOVERY_IVL 9
#define ZMQ_SNDBUF 11
#define ZMQ_RCVBUF 12
#define ZMQ_RCVMORE 13
#define ZMQ_FD 14
#define ZMQ_EVENTS 15
#define ZMQ_TYPE 16
#define ZMQ_LINGER 17
#define ZMQ_RECONNECT_IVL 18
#define ZMQ_BACKLOG 19
#define ZMQ_RECONNECT_IVL_MAX 21
#define ZMQ_MAXMSGSIZE 22
#define ZMQ_SNDHWM 23
#define ZMQ_RCVHWM 24
#define ZMQ_MULTICAST_HOPS 25
#define ZMQ_RCVTIMEO 27
#define ZMQ_SNDTIMEO 28
#define ZMQ_LAST_ENDPOINT 32
#define ZMQ_ROUTER_MANDATORY 33
#define ZMQ_TCP_KEEPALIVE 34
#define ZMQ_TCP_KEEPALIVE_COUNT 35
#define ZMQ_TCP_KEEPALIVE_IDLE 36
#define ZMQ_TCP_KEEPALIVE_INTRVL 37
#define ZMQ_TCP_ACCEPT_FILTER 38
#define ZMQ_IMMEDIATE 39
#define ZMQ_XPUB_VERBOSE 40
#define ZMQ_ROUTER_RAW 41
#define ZMQ_IPV6 42
#define ZMQ_MECHANISM 43
#define ZMQ_PLAIN_SERVER 44
#define ZMQ_PLAIN_USERNAME 45
#define ZMQ_PLAIN_PASSWORD 46
#define ZMQ_CURVE_SERVER 47
#define ZMQ_CURVE_PUBLICKEY 48
#define ZMQ_CURVE_SECRETKEY 49
#define ZMQ_CURVE_SERVERKEY 50
#define ZMQ_PROBE_ROUTER 51
#define ZMQ_REQ_CORRELATE 52
#define ZMQ_REQ_RELAXED 53
#define ZMQ_CONFLATE 54
#define ZMQ_ZAP_DOMAIN 55
|#

;; Socket options.
(define ZMQ/AFFINITY                4)
(define ZMQ/IDENTITY                5)
(define ZMQ/SUBSCRIBE               6)
(define ZMQ/UNSUBSCRIBE             7)
(define ZMQ/RATE                    8)
(define ZMQ/RECOVERY-INTERVAL       9)
(define ZMQ/SEND-BUFFER            11)
(define ZMQ/RECEIVE-BUFFER         12)
(define ZMQ/RECEIVE-MORE           13)
(define ZMQ/FD                     14)
(define ZMQ/EVENTS                 15)
(define ZMQ/TYPE                   16)
(define ZMQ/LINGER                 17)
(define ZMQ/RECONNECT-INTERVAL     18)
(define ZMQ/BACKLOG                19)
(define ZMQ/RECONNECT-INTERVAL-MAX 21)
(define ZMQ/MESSAGE-SIZE-MAX       22)
(define ZMQ/SEND-HIGH-WATER        23)
(define ZMQ/RECEIVE-HIGH-WATER     24)
(define ZMQ/MULTICAST_HOPS         25)
(define ZMQ/RECEIVE-TIMEOUT        27)
(define ZMQ/SEND-TIMEOUT           28)
(define ZMQ/LAST-ENDPOINT          32)
(define ZMQ/ROUTER-MANDATORY       33)
(define ZMQ/TCP-KEEPALIVE          34)
(define ZMQ/TCP-KEEPALIVE-COUNT    35)
(define ZMQ/TCP-KEEPALIVE-IDLE     36)
(define ZMQ/TCP-KEEPALIVE-INTERVAL 37)
(define ZMQ/TCP-ACCEPT-FILTER      38)
(define ZMQ/IMMEDIATE              39)
(define ZMQ/XPUB-VERBOSE           40)
(define ZMQ/ROUTER-RAW             41)
(define ZMQ/IPV6                   42)
(define ZMQ/MECHANISM              43)
(define ZMQ/PLAIN-SERVER           44)
(define ZMQ/PLAIN-USERNAME         45)
(define ZMQ/PLAIN-PASSWORD         46)
(define ZMQ/CURVE-SERVER           47)
(define ZMQ/CURVE-PUBLIC-KEY       48)
(define ZMQ/CURVE-SECRET-KEY       49)
(define ZMQ/CURVE-SERVER-KEY       50)
(define ZMQ/PROBE-ROUTER           51)
(define ZMQ/REQUEST-CORRELATE      52)
(define ZMQ/REQUEST-RELAXED        53)
(define ZMQ/CONFLATE               54)
(define ZMQ/ZAP-DOMAIN             55)
  
;; Send/receive options.
(define ZMQ/NO-WAIT   1)
(define ZMQ/SEND-MORE 2)

;; Socket event codes
(define ZMQ/POLLIN  1)
(define ZMQ/POLLOUT 2)
(define ZMQ/POLLERR 3)

;; zmq error codes.
(define EADDRINUSE 48)
(define EADDRNOTAVAIL 49)
(define EAGAIN 35)
(define EFAULT 14)
(define EFSM 156384763)
(define EHOSTUNREACH 65)
(define EINTR 4)
(define EMFILE 24)
(define EMTHREAD 156384766)
(define ENOCOMPATPROTO 156384764)
(define ENODEV 19)
(define ENOTSOCK 38)
(define ENOTSUP 45)
(define EPROTONOSUPPORT 43)
(define ETERM 156384765)
