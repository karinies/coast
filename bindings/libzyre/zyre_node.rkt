#lang racket/base

(require
 ffi/unsafe
 "../libczmq/libczmq.rkt"
 "libzyre.rkt")

(provide
 zyre/node/new
 zyre/node/destroy
 zyre/join
 zyre/leave
 zyre/receive
 zyre/whisper
 zyre/shout
 zyre/socket)
 
;; Constructor.
(define-zyre-function zyre/node/new "zyre_node_new" (_fun _zcontext _zsocket -> _zyre-node))
;; Destructor.
(define-zyre-function zyre/node/destroy "zyre_node_destroy" (_fun (_ptr i _zyre-node) -> _void))
;; Tracing on/off. Unimplemented as of 2013-11-04.
;(define-zyre-function zre/node/trace "zre_node_set_verbose" (_fun _zyre-node _bool -> _void))

;; Join a group.
(define-zyre-function zyre/join "zyre_join" (_fun _zyre-node _string -> (r : _int))) ; Always returns 0.

;; Leave a group.
(define-zyre-function zyre/leave "zyre_leave" (_fun _zyre-node _string -> _int)) ; Always returns 0.

;; Receive the next message.
(define-zyre-function zyre/receive "zyre_recv" (_fun _zyre-node -> _zyre-message))
;; Send a message to a single peer whose peer id is in the first frame of the message.
;; Routine destroys message after sending.
(define-zyre-function zyre/whisper "zyre_whisper" (_fun _zyre-node (_ptr i _zyre-message) -> _int)) ; Always returns 0.
;; Broadcast a message to a group of peers. Destroys message post-broadcast?
(define-zyre-function zyre/shout "zyre_shout" (_fun _zyre-node (_ptr i _zyre-message) -> _int)) ; Always returns 0.
;; Return the zyre pipe for polling.
(define-zyre-function zyre/socket "zyre_socket" (_fun _zyre-node -> _zsocket))
