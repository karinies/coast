#lang racket/base

(require "zmq.rkt")

(provide client server)

(define (server)
  (let* ((context (zmq/context/new))
         (responder (zmq/socket/new context 'REPLY)))
    (zmq/socket/bind responder "tcp://*:5555")
    (let loop ((buffer (make-bytes 10 0)))
      (zmq/receive responder buffer)
      (printf "Received hello\n")
      (zmq/send responder #"world")
      (bytes-fill! buffer 0)
      (loop buffer))))


(define (client)
  (printf "Connecting to the world server ...\n")
  (let* ((context (zmq/context/new))
         (requestor (zmq/socket/new context 'REQUEST)))
    (zmq/connect requestor "tcp://localhost:5555")
    (let loop ((i 0) (buffer (make-bytes 10 0)))
      (when (< i 10)
        (printf "sending hello ~a ...\n" i)
        (zmq/send requestor #"hello")
        (zmq/receive requestor buffer)
        (printf "received world ~a ...\n" i)
        (loop (add1 i) buffer)))
    (zmq/socket/close requestor)
    (zmq/context/close context)))
    

#| To launch the server on the command line:
      racket -l racket/base -t hwserver.rkt -e '(server)' &
   To launch the client on the command line:
      racket -l racket/base -t hwserver.rkt -e '(client)' &
|#