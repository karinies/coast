#lang racket/base

(require
 ffi/unsafe
 "../libczmq/libczmq.rkt"
 "libzyre.rkt")

(provide
 zyre/log/new
 zyre/log/connect
 zyre/log/advisory
 zyre/log/warning
 zyre/log/error)

(define-cpointer-type _zyre-log-message)

;; Log constructor.
(define-zyre-function zyre/log/new "zyre_log_new" (_fun _zcontext (id : _string) -> _zyre-log))
;; Log destructor.
(define-zyre-function zyre/log/destroy "zyre_log_destroy" (_fun (_ptr i _zyre-log) -> _void))
;; Connect log to remote endpoint.
(define-zyre-function zyre/log/connect "zyre_log_connect" (_fun _zyre-log (endpoint : _string) -> _void))

;; Accept anything, convert it to a string, and snip to a max of 255 characters.
(define (snip m)
  (cond
    ((bytes? m)
     (bytes->string/utf-8 (if (> (bytes-length m) 255)  (subbytes m 0 255) m)))
    ((string? m)          (if (> (string-length m) 255) (substring m 0 255) m))
    (else
     (let* ((o (open-output-string))
            (s (begin (display m o) (get-output-string o))))
       (if (> (string-length s) 255) (substring s 0 255) s)))))

;; For all three below the message must be 255 characters or less.
;; Broadcast a log information message.
(define-zyre-function
  zyre-log-info "zyre_log_info"
  (_fun _zyre-log (event : _int) (peer : _string) (message : _string) -> _void))
(define (zyre/log/advisory event peer message) (zyre-log-info event peer (snip message)))
;; Broadcast a log warning message.
(define-zyre-function
  zyre-log-warning "zyre_log_warning"
  (_fun _zyre-log (event : _int) (peer : _string) (message : _string) -> _void))
(define (zyre/log/warning event peer message) (zyre-log-warning event peer (snip message)))
;; Broadcast a log error message.
(define-zyre-function
  zyre-log-error "zyre_log_error"
  (_fun _zyre-log (event : _int) (peer : _string) (message : _string) -> _void))
(define (zyre/log/error event peer message) (zyre-log-error event peer message))

;; Log messages.
(define-zyre-function zyre/log/message/new "zre_log_msg_new" (_fun _int -> _zyre-log-message))
(define-zyre-function zyre/log/message/destroy "zre_log_msg_destroy" (_fun (_ptr i _zyre-log-message) -> _void))
(define-zyre-function zyre/log/message/receive "zre_log_msg_recv" (_fun _zsocket -> _zyre-log-message))
;(define-zyre-function zyre/log/message/send