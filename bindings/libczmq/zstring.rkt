#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 zstring/receive
 zstring/receive/try
 zstring/send)

;; Blocking receive.
(define-czmq-function zstring/receive "zstr_recv" (_fun _zsocket -> _string))
;; Nonblocking receive.
(define-czmq-function zstring/receive/try "zstr_recv_nowait" (_fun _zsocket -> _string))
;; Transmit a string. Returns #t on success and #f on if there was an error.
(define-czmq-function zstring/send "zstr_send" (_fun _zsocket _string -> (r : _int = (zero? r))))
