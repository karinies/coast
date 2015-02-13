#lang racket/base

(require
 (only-in typed/racket/base assert)
 "../../libzmq/zmq.rkt"
 "../zcontext.rkt"
 "../zpoller.rkt"
 "../zsocket.rkt"
 "../zstring.rkt")

(provide zpoller/test)

(define (zpoller/test)
  (display " * zpoller: ")
  (let* ((context (zcontext/new))
         (vent (zsocket/new context 'PUSH))
         (port (zsocket/bind vent "tcp://*:*"))
         (sink (zsocket/new context 'PULL)))
    (zmq/assert/0... 'zsocket/bind port)
    (assert (zsocket/connect sink (format "tcp://localhost:~a" port)))
  
    ; Construct a poller.
    (let* ((bowl (zsocket/new context 'PULL))
           (dish (zsocket/new context 'PULL))
           (poller (zpoller/new bowl dish)))
      (assert poller)
      ; Add another reader to the existing poller.
      (assert (zpoller/add poller sink))
      
      (zstring/send vent "Hello World")
      ; We expect a message only on the sink.
      (let ((which (zpoller/wait poller -1)))
        (assert (equal? which sink))
        (assert (not (zpoller/expired? poller)))
        (assert (not (zpoller/terminated? poller)))
        (assert (string=? (zstring/receive sink) "Hello World")))
      
      (zpoller/destroy poller)
      (zsocket/destroy context sink)
      (zsocket/destroy context vent)
      (zsocket/destroy context bowl)
      (zsocket/destroy context dish)
      (zcontext/destroy context)
      
      (displayln "OK")
      #t)))


;; racket -l racket/base -t zpoller.rkt -e '(zpoller/test)' &
