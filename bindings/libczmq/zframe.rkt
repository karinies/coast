#lang racket/base

(require
 ffi/unsafe
 (only-in "../libzmq/zmq.rkt" zmq/assert/0 zmq/assert/0...)
 "libczmq.rkt")

(provide
 zframe/new
 zframe/empty/new
 zframe/destroy
 zframe/duplicate
 zframe/size
 zframe/data
 zframe/receive
 zframe/receive*
 zframe/send
 zframe/send*
 zframe/more
 zframe/more?
 zframe=?
 zframe/reset)

;; We include a few zframe functions from czmq for use as building blocks for
;; more convenient Racket functions.

;; Flags for transmitting zframes
(define ZFRAME/MORE     1) ; More zframes to follow.
(define ZFRAME/REUSE    2) ; Do not destroy the zframe.
(define ZFRAME/DONTWAIT 4) ; Nonblocking transmission of the zframe.

;; Allocate a fresh zframe with payload given by byte string b.
;; The new frame contains a copy of b.
(define-czmq-function zframe/new "zframe_new" (_fun (b : _bytes) (_size_t = (bytes-length b)) -> _zframe))
;; Allocate an empty fresh zframe.
(define-czmq-function zframe/empty/new "zframe_new_empty" (_fun -> _zframe))
;; Return a zframe to the heap from which it came.
(define-czmq-function zframe/destroy "zframe_destroy" (_fun (_ptr i _zframe) -> _void))
;; Duplicate a zframe.
(define-czmq-function zframe/duplicate "zframe_dup" (_fun _zframe -> _zframe))

;; Return the size of the zframe payload in bytes.
(define-czmq-function zframe/size "zframe_size" (_fun _zframe -> _size_t))
;; Return the payload of zframe f as a byte string.
(define-czmq-function zframe/data "zframe_data" (_fun (f : _zframe) -> (_bytes o (zframe/size f))))

;; Blocking receive for a single zframe. Returns C NULL if the receive was interrupted.
(define-czmq-function zframe/receive "zframe_recv" (_fun _zsocket -> _zframe))
;; Nonblocking receive for a single zframe.
(define-czmq-function zframe/receive* "zframe_recv_nowait" (_fun _zsocket -> _zframe))

(define-czmq-function
  zframe/send/raw "zframe_send"
  (_fun (_ptr i _zframe) _zsocket (flags : _int) -> (r : _int = (zero? r))))

;; Blocking send of zframe.
(define (zframe/send frame socket #:more [more #f] #:reuse [reuse #f])
  (zframe/send/raw
   frame socket
   (bitwise-ior (if more ZFRAME/MORE 0) (if reuse ZFRAME/REUSE 0))))

;; Nonblocking transmission of a zframe.
(define (zframe/send* frame socket #:more [more #f] #:reuse [reuse #f])
  (zframe/send/raw
   frame socket
   ;(bitwise-ior (if more ZFRAME/MORE 0) (if reuse ZFRAME/REUSE 0) ZFRAME/DONTWAIT)))
   (+ (if more ZFRAME/MORE 0) (if reuse ZFRAME/REUSE 0) ZFRAME/DONTWAIT)))

;; Return the MORE flag in the zframe as #t or #f.
(define-czmq-function zframe/more? "zframe_more" (_fun _zframe -> _bool))
;; Set the MORE flag of the zframe.
(define-czmq-function zframe/more/set "zframe_set_more" (_fun _zframe _bool -> _void))
(define zframe/more
  (case-lambda
    ((f)   (zframe/more?    f))
    ((f b) (zframe/more/set f b))))

;; Return #t if two zframes have identical sizes and payloads, otherwise #f.
(define-czmq-function zframe=? "zframe_eq" (_fun _zframe _zframe -> _bool))

;; Reset the payload of a zframe.
(define-czmq-function zframe/reset "zframe_reset" (_fun _zframe (b : _bytes) (n : _int = (bytes-length b)) -> _void))
