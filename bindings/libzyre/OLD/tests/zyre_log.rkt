#lang racket/base

(require
 (only-in
  "../../libczmq/czmq.rkt"
  zcontext/new
  zsocket/bind
  zsocket/new
  zsocket/subscribe)
 "../zyre_log.rkt")

;; I'm not sure that I really need the zyre log functions anyway 2013.11.12

(define (zyre/log/test)
  (printf " * zyre_log: ")
  (let* ((context (zcontext/new))
         ; Collects all log messages.
         (collector (zsocket/new context 'SUBSCRIBE)))
    (zsocket/bind collector "tcp://127.0.0.1:5555")
    (zsocket/subscribe collector "")
    ;; Create a log instance to transmit log messages.
    (let ((logger (zyre/log/new context "paul bunyan")))
      #f)))



