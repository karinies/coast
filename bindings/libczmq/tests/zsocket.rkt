#lang racket/base

(require
 (only-in typed/racket/base assert)
 "../../libzmq/zmq.rkt"
 "../zcontext.rkt"
 "../zframe.rkt"
 "../zsocket.rkt"
 "../zstring.rkt")

(provide zsocket/test)

(define interface "*")
(define domain "localhost")
(define port 5560)

(define (zsocket/test)
  (printf " * zsocket: ")
  (let* ((context (zcontext/new))
         (writer (zsocket/new context 'PUSH))
         (reader (zsocket/new context 'PULL)))
    (assert context)
    (assert writer)
    (assert reader)
    (assert (string=? (zsocket/flavor writer) "PUSH"))
    (assert (string=? (zsocket/flavor reader) "PULL"))

    (let* ((transport (format "tcp://~a:~a" interface port))
           (service (zsocket/bind writer transport))) ; zsocket/bind returns the TCP port on success.
      (assert (= service port))
      ; Check zsocket/unbind.
      (assert (zsocket/unbind writer transport))
      (sleep 0.1); Sleep for 100 milliseconds to guarantee that the OS releases the port for reuse.
    
      ; Bind again.
      (assert (= port (zsocket/bind writer transport)))
      
      ; Note that we CONNECT to an IP address localhost:5560 and not a host interface.
      (assert (zsocket/connect reader (format "tcp://~a:~a" domain port)))
      (zstring/send writer "HELLO")
      (assert (string=? (zstring/receive reader) "HELLO"))
       
      ; Test binding to ports.
      (let ((port (zsocket/bind writer (format "tcp://~a:*" interface))))
        (zmq/assert/0... 'zsocket/bind port))
      (assert (not (zsocket/poll writer 100)))
      ; Test error state when connecting to an invalid socket type.
      ; ("txp://..." instead of "tcp://..." is a deliberate error).
      (assert (not (zsocket/connect reader (format "txp://~a:~a" domain port))))
      
      ; Test sending frames to socket.
      (assert (zsocket/send writer #"ABC" #:more #t))
      (assert (zsocket/send writer #"DEFG"))
      (let ((f (zframe/receive reader)))
        (assert f)
        ;(assert (bytes=? #"ABC" (zframe/payload f)))
        (assert (bytes=? #"ABC" (zframe/data f)))
        (assert (zframe/more? f))
        (zframe/destroy f))
      (let ((f (zframe/receive reader)))
        (assert f)
        ;(assert (bytes=? #"DEFG" (zframe/payload f)))
        (assert (bytes=? #"DEFG" (zframe/data f)))
        (assert (not (zframe/more? f)))
        (zframe/destroy f))
      
      (zsocket/destroy context reader)
      (zsocket/destroy context writer)
      (zcontext/destroy context)
      
      (displayln "OK")
      #t)))

;; racket -l racket/base -t zsocket.rkt -e '(zsocket/test)'