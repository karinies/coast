#lang racket/base

(require
 ffi/unsafe
 "libzyre.rkt"
 "../libczmq/libczmq.rkt")

(provide
 zyre/new
 zyre/destroy
 zyre/receive
 zyre/join
 zyre/leave
 zyre/whisper
 zyre/island/whisper
 zyre/shout
 zyre/socket
 zyre/header/set
 zyre/start
 zyre/stop)

;; Construct and wrap a fresh zyre node.
(define-zyre-function zyre/new "zyre_new" (_fun _zcontext -> _zyre))
;; Destructor.
(define-zyre-function zyre/destroy "zyre_destroy" (_fun (_ptr i _zyre) -> _void))
;; Blocking receive for a message sent to the zyre node.
(define-zyre-function zyre/receive "zyre_recv" (_fun (self : _zyre) -> _zmessage))
;; Join a group.
(define-zyre-function zyre/join "zyre_join" (_fun (self : _zyre) (group : _string) -> (r : _int) -> (zero? r)))
;; Leave a group.
(define-zyre-function zyre/leave "zyre_leave" (_fun (self : _zyre) (group : _string) -> (r : _int) -> (zero? r)))

;; Message transmission. In each case the message is DESTROYED after transmission by zyre!

;; Transmit a message to the given peer.
;(define-zyre-function zyre/whisper "zyre_whisper" (_fun (self : _zyre) (peer : _string) (_ptr i _zmessage) -> (r : _int) -> (zero? r)))
(define-zyre-function
  zyre/whisper "zyre_whisper"
  (_fun (self : _zyre) (peer : _string) (_ptr io _zmessage) -> (r : _int) -> (zero? r)))

;; Custom-built for transmitting a WHISPER to a given peer since every time
;; I call zyre/whisper Racket crashes.
(define-zyre-function
  zyre/island/whisper "zyre_island_whisper"
  (_fun (self : _zyre) (peer : _string)
        (frame1 : _bytes) (_size_t = (bytes-length frame1))
        (frame2 : _bytes) (_size_t = (bytes-length frame2))
        -> (r : _int) -> (zero? r)))

;; Transmit a message to the peers who are members of the given group.
(define-zyre-function zyre/shout "zyre_shout" (_fun (self : _zyre) (group : _string) (_ptr i _zmessage) -> (r : _int) -> (zero? r)))

;; Return the czmq socket that the node uses to communicate with the underlying thread animating the node.
(define-zyre-function zyre/socket "zyre_socket" (_fun _zyre -> _zsocket))

;; Set a header value for the node.
(define-zyre-function zyre/header/set "zyre_set_header" (_fun _zyre (key : _string) (value : _string) -> _void))

;; Start the zyre node after setting headers (if any).
;; Once started the node begins beaconing, discovery and connection.
(define-zyre-function zyre/start "zyre_start" (_fun _zyre -> _void))
;; Stop the zyre node. Merely a courtesy as it suffices to destroy the node.
(define-zyre-function zyre/stop "zyre_stop" (_fun _zyre -> _void))

