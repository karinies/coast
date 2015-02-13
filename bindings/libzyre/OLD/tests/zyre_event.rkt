#lang racket/base

(require
  (only-in typed/racket/base assert)
 "../zyre_event.rkt"
 "../peer.rkt"
 "../../libczmq/zcontext.rkt"
 "../../libczmq/zframe.rkt"
 "../../libczmq/zmessage.rkt"
 )

(provide zyre/event/test)

(define (zyre/event/test)
  (display " * zyre_event: ")
  (let* ((context (zcontext/new))
         (alice (zyre/new context))
         (bob   (zyre/new context)))
    (zyre/header/set alice "X-FILEMQ" "tcp://128.0.0.1:6777")
    (zyre/header/set alice "X-HELLO" "World")
    (zyre/start alice)
    (zyre/start bob)
    (zyre/join alice "GLOBAL")
    (zyre/join bob   "GLOBAL")
    
    ; Give alice and bob time to find one another.
    (sleep 0.25)

    ; Alice shouts to GLOBAL.
    (let ((m (zmessage/new)))
      (zmessage/string/add m "Hello World")
      (zyre/shout alice "GLOBAL" m))
    
    ; Parse an ENTER.
    (let* ((e (zyre/event/receive bob))
           (flavor (zyre/event/flavor e))
           (sender (zyre/event/sender e)))
      (assert (eq? flavor 'ZYRE/ENTER))
      (displayln sender)
      (assert (string=? "World" (zyre/event/header e "X-HELLO")))
      (zyre/event/destroy e))
    
    ; Parse a JOIN.
    (let* ((e (zyre/event/receive bob))
           (flavor (zyre/event/flavor e)))
      (assert (eq? flavor 'ZYRE/JOIN))
      (zyre/event/destroy e))
    
    ; Parse a SHOUT.
    (let* ((e (zyre/event/receive bob))
           (flavor (zyre/event/flavor e))
           (group (zyre/event/group e))
           (m (zyre/event/message e))
           (f (zmessage/pop m))) ; A zframe.
      (assert (eq? flavor 'ZYRE/SHOUT))
      (assert (string=? "GLOBAL" group))
      (assert (bytes=? (zframe/payload f) #"Hello World"))
      (zframe/destroy f)
      (zyre/event/destroy e))
 #t))


;; racket -l racket/base -t zyre_event.rkt -e '(zyre/event/test)' &