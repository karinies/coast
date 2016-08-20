#lang racket/base

(require
 racket/contract/base
 "../bindings/libczmq/zcontext.rkt"
 "../bindings/libczmq/zsocket_options.rkt"
 "../bindings/libczmq/zpoller.rkt"
 "../bindings/libzmq/zmq.rkt"
 "../bindings/libzyre/libzyre.rkt"
 "../bindings/libzyre/peer.rkt"
 "../bindings/libzyre/zyre_event.rkt"
 "../islet.rkt"
 "../ports.rkt"
 "base.rkt"
 "logger.rkt"
)

(provide
 (contract-out
  [bridge/worker thunk/c]))


#|
2014-06-09

I've tried just about everything I can think of to eliminate busy waiting
in the bridge between an island and the ZyRE peer underneath it but so far
nothing seems to work.

At first I naively expected that simply extracting the file descriptor of the
ZMQ_PIPE that connects the ZyRE peer to the island would be sufficent and
that by using scheme_fd_to_input_port or scheme_fd_to_semaphore I could
integrate the pipe into Racket (sync ...). Both of these approaches failed.

On inspecting the source code for ZMQ polling
(see signaller.cpp, poll.cpp, and zmp.cpp in the ZMQ source)
I realized that the Linux build uses eventfd for reflecting the status
of a ZMQ pipe back to the poller.
Apparently, the implementations of scheme_fd_to_input_port and scheme_fd_to_semaphore
simply don't respond to a file descriptor generated by eventfd.

However, specifying
    ./configure --disable-eventfd
forces libzmq to revert to socketpair as the generic mechanism for reflecting
pipe state. Aha! Problem solved. Alas no, as the mechanisms of
scheme_fd_to_input_port and scheme_fd_to_semaphore don't seem to accommodate a
file descriptor that refers to the read end of a socketpair (for reasons that I
don't understand).

Note: On using scheme_fd_to_semaphore see
http://lists.racket-lang.org/users/archive/2013-April/057410.html
(which doesn't seem to work in our case).

I just don't have the time to fuss with this any longer.
The most appealing solution is to modify either scheme_fd_to_input_port or scheme_fd_to_semaphore
to accommodate socketpair file descriptors.

|#

;; On using scheme_fd_to_semaphore
;; http://lists.racket-lang.org/users/archive/2013-April/057410.html
;; (which doesn't seem to work in our case).

(define (zyre/event/available? socket)
  (positive? (bitwise-and (zsocket/events socket) ZMQ/POLLIN)))

(define (bridge/worker)
  (let* ([i (this/island)]
         [underpeer (island/underpeer i)]
         [socket   (zyre/socket underpeer)]
         [poller   (zpoller/new socket)]
         [nickname (log/name/build (island/nickname i) 'bridge)]
         [ingress (island/ingress i)])    
    (let loop ()
      (cond
        [(zpoller/wait poller 0)
         ; At least one message is waiting for us.
         (let* ([e      (zyre/event/new underpeer)]
                [flavor (zyre/event/flavor e)]
                [sender (zyre/event/sender e)]) ; UUID of transmitting ZyRE node.
           (cond
             [(eq? flavor 'ZYRE/WHISPER)
              ;(log/info nickname "saw WHISPER" #t)
              (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                (log/fatal nickname "ingress thread is dead" #t))]
             
             [(eq? flavor 'ZYRE/ENTER)
              ;(log/info nickname "saw ENTER" #t)
              (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                (log/fatal nickname "ingress thread is dead" #t))]
             
             [(eq? flavor 'ZYRE/EXIT)
              ;(log/info nickname "saw EXIT" #t)
              (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                (log/fatal nickname "ingress is dead" #t))
              (zyre/event/destroy e)]
             
             [else ; Ignore JOIN, LEAVE, and SHOUT events.
              (zyre/event/destroy e)]))
         (loop)] ; Try to read another message (if any)
        
        [else
         (sleep 0) ; Yield the processor.
         (loop)]))))

#|

(define (bridge/worker-v4) ; This one works but is effectively busy waiting.
  (let* ([i (this/island)]
         [underpeer (island/underpeer i)]
         [socket   (zyre/socket underpeer)]
         [poller   (zpoller/new socket)]
         [nickname (log/name/build (island/nickname i) 'bridge)]
         [ingress (island/ingress i)])    
    (let loop ()
      (cond
        [(zpoller/wait poller 0)
         ; At least one message is waiting for us.
         (let* ([e      (zyre/event/new underpeer)]
                [flavor (zyre/event/flavor e)]
                [sender (zyre/event/sender e)]) ; UUID of transmitting ZyRE node.
           (cond
             [(eq? flavor 'ZYRE/WHISPER)
              ;(log/info nickname "saw WHISPER" #t)
              (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                (log/fatal nickname "ingress thread is dead" #t))]
             
             [(eq? flavor 'ZYRE/ENTER)
              ;(log/info nickname "saw ENTER" #t)
              (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                (log/fatal nickname "ingress thread is dead" #t))]
             
             [(eq? flavor 'ZYRE/EXIT)
              ;(log/info nickname "saw EXIT" #t)
              (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                (log/fatal nickname "ingress is dead" #t))
              (zyre/event/destroy e)]
             
             [else ; Ignore JOIN, LEAVE, and SHOUT events.
              (zyre/event/destroy e)]))
         ; Try to read another message (if any).
         ;(log/info nickname "zyre/event/available?" (zyre/event/available? socket))
         (loop)]
        
        [else
         ;(display "yielding the processor\n")
         (sleep 0) ; Yield the processor.
         (loop)]))))
  
  
(define (bridge/worker-v3)
  (let* ([i (this/island)]
         [underpeer (island/underpeer i)]
         [socket    (zyre/socket underpeer)] ; ZMQ pipe socket to the underpeer
         [fd        (zsocket/fd socket)] ; Unix file descriptor for the ZMQ pipe socket.
         [nickname (log/name/build (island/nickname i) 'bridge)]
         [ingress (island/ingress i)])
    (displayln fd) ;(displayln input)
    (displayln (read-byte (file-descriptor-to-input-port fd 'test #f #f)))
    
    (let loop ([s (file-descriptor-to-semaphore fd MZFD_CREATE_READ #t)])
      (displayln s)
      (sync s)
      (display "sync fired inside main loop\n")
      ; At this point we know there is at least 1 byte of intput available on the pipe.
      (let read ()
        (display "inside read loop\n")
        (when (zyre/event/available? socket)
          ; There is a full message available.
          (let* ([e      (zyre/event/new underpeer)]
                 [flavor (zyre/event/flavor e)]
                 [sender (zyre/event/sender e)]) ; uuid of transmitting ZyRE node.
            (cond
              [(eq? flavor 'ZYRE/WHISPER)
               (log/info nickname "saw WHISPER" #t)
               (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                 (log/fatal nickname "ingress thread is dead" #t))]
              
              [(eq? flavor 'ZYRE/ENTER)
               (log/info nickname "saw ENTER" #t)
               (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                 (log/fatal nickname "ingress thread is dead" #t))]
              
              [(eq? flavor 'ZYRE/EXIT)
               (log/info nickname "saw EXIT" #t)
               (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                 (log/fatal nickname "ingress is dead" #t))
               (zyre/event/destroy e)]
              
              [else ; Ignore JOIN, LEAVE, and SHOUT events.
               (zyre/event/destroy e)]))
          (read))) ; Try to read another message (if any).
      (loop (file-descriptor-to-semaphore fd MZFD_CREATE_READ #t)))))

(define (bridge/worker-v2)
  (let* ([i (this/island)]
         [underpeer (island/underpeer i)]
         [socket    (zyre/socket underpeer)] ; ZMQ pipe socket to the underpeer
         [fd        (zsocket/fd socket)] ; Unix file descriptor for the ZMQ socket.
         [input     (file-descriptor-to-semaphore fd 1 1)] ; 1 = MZFD_CREATE_READ 1 = is_socket
         [nickname (log/name/build (island/nickname i) 'bridge)]
         [ingress (island/ingress i)])
      (displayln fd) (displayln input)
      (let pipe ([in (sync input)] [turn 0])
        (display (format "inside pipe loop: ~a\n" turn))
        ; At this point we know there is at least 1 byte of intput available on the pipe.
        (let poll ([flag (zyre/event/available? socket)] [total 0])
          (display (format "inside pool loop flag:~a\n" flag))
          (when flag
            ; There is a full message available.
            (let* ([e      (zyre/event/new underpeer)]
                   [flavor (zyre/event/flavor e)]
                   [sender (zyre/event/sender e)]) ; uuid of transmitting ZyRE node.
              (cond
                [(eq? flavor 'ZYRE/WHISPER)
                 (log/info nickname "saw WHISPER" (metrics turn total))
                 (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                   (log/fatal nickname "ingress thread is dead" (metrics turn total)))]
                
                [(eq? flavor 'ZYRE/ENTER)
                 (log/info nickname "saw ENTER" (metrics turn total))
                 (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                   (log/fatal nickname "ingress thread is dead" (metrics turn total)))]
                
                [(eq? flavor 'ZYRE/EXIT)
                 (log/info nickname "saw EXIT" (metrics turn total))
                 (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                   (log/fatal nickname "ingress is dead" (metrics turn total)))
                 (zyre/event/destroy e)]
                
                [else ; Ignore JOIN, LEAVE, and SHOUT events.
                 (zyre/event/destroy e)]))
            ; Continue reading messages until failure.
            (poll (zyre/event/available? socket) (add1 total))))

        (pipe (sync input)(add1 turn)))))


;; This version tries to do a (sync ...) on the file descriptor of the pipe.
(define (bridge/worker-v1)
  (let* ([i (this/island)]
         [underpeer (island/underpeer i)]
         [socket    (zyre/socket underpeer)] ; ZMQ pipe socket to the underpeer
         [fd        (zsocket/fd socket)] ; Unix file descriptor for the ZMQ socket.
         ;[input     (file/descriptor-to-port/input fd 'bridge/inbound)] ; Input port for read side of the underpeer ZMQ pipe.
         [nickname (log/name/build (island/nickname i) 'bridge)]
         [ingress (island/ingress i)])
    (let-values ([(input output) (file/descriptor-to-ports fd 'bridge/pipe)])
      (displayln fd) (displayln input)
      (let pipe ([in (sync input)] [turn 0])
        (display (format "inside pipe loop: ~a\n" turn))
        ; At this point we know there is at least 1 byte of intput available on the pipe.
        (let poll ([flag (zyre/event/available? socket)] [total 0])
          (display (format "inside pool loop flag:~a\n" flag))
          (when flag
            ; There is a full message available.
            (let* ([e      (zyre/event/new underpeer)]
                   [flavor (zyre/event/flavor e)]
                   [sender (zyre/event/sender e)]) ; uuid of transmitting ZyRE node.
              (cond
                [(eq? flavor 'ZYRE/WHISPER)
                 (log/info nickname "saw WHISPER" (metrics turn total))
                 (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                   (log/fatal nickname "ingress thread is dead" (metrics turn total)))]
                
                [(eq? flavor 'ZYRE/ENTER)
                 (log/info nickname "saw ENTER" (metrics turn total))
                 (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                   (log/fatal nickname "ingress thread is dead" (metrics turn total)))]
                
                [(eq? flavor 'ZYRE/EXIT)
                 (log/info nickname "saw EXIT" (metrics turn total))
                 (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                   (log/fatal nickname "ingress is dead" (metrics turn total)))
                 (zyre/event/destroy e)]
                
                [else ; Ignore JOIN, LEAVE, and SHOUT events.
                 (zyre/event/destroy e)]))
            (poll (zyre/event/available? socket) (add1 total)))) 
        (pipe (sync input)(add1 turn))))))

; Bounds on snooze time in milliseconds.
(define SNOOZE/MIN 100)
(define SNOOZE/MAX 500)
;; Snooze time decreases faster than it increases.
(define SNOOZE/DOWN 100)
(define SNOOZE/UP   150)
;; traffic - #t if we see traffic this round and #f otherwise
;; snooze - latest sleep time in milliseconds.
;; Returns the number of milliseconds to sleep in the range [10, 100].
;; If we saw at least one event then reduce the snooze time.
;; If we failed to see any events then increase the snooze time.
(define (snooze/adjust traffic snooze)
  (if (positive? traffic)
      (max SNOOZE/MIN (- snooze SNOOZE/DOWN))
      (min SNOOZE/MAX (+ snooze SNOOZE/UP))))


;; A simpler version that effectively yields the processor whenever it runs out of ZyRE events to process.
;; A thunk to be executed by a dedicated islet.
(define (xbridge/worker)
  (let ([i (this/island)])
    (let* ([underpeer (island/underpeer i)]
           [poller (zpoller/new (zyre/socket underpeer))]
           [nickname (log/name/build (island/nickname i) 'bridge)]
           [ingress (island/ingress i)])
      ; Check for complete ZyRE message. 0 means nonblocking.
      ; ZeroMQ polling is edge-triggered (see http://funcptr.net/2012/09/10/zeromq---edge-triggered-notification/)
      ; so that once it triggers we must repeatedly poll to pick up all waiting messages.
      (let loop ([traffic 0]   ; Count of number of messages seen so far in this polling period.
                 [total 0])     ; Total number of messages seen over the lifespan of the bridge.
                 ;[snooze 100]) ; Initial sleep time in milliseconds.
        (cond
          [(thread-try-receive)
           =>
           (lambda (m)
             (cond
               [(eq? m 'TERMINATE)
                (log/debug nickname "terminating" total)
                (zpoller/destroy poller)]
               [else  ; Ignore anything else for now.
                (loop traffic total)]))]
          
          [(zpoller/wait poller 1) ; #t if any ZyRE messages are enqueued and #f otherwise.
           (let* ([e      (zyre/event/new underpeer)]
                  [flavor (zyre/event/flavor e)]
                  [sender (zyre/event/sender e)]) ; uuid of transmitting ZyRE node.
             (cond
               [(eq? flavor 'ZYRE/WHISPER)
                (log/info nickname "saw WHISPER" total)
                (unless (thread-send ingress (vector-immutable 'WHISPER sender e) #f)
                  (log/fatal nickname "ingress thread is dead" total))]

               [(eq? flavor 'ZYRE/ENTER)
                (log/info nickname "saw ENTER" total)
                (unless (thread-send ingress (vector-immutable 'ENTER sender e) #f)
                  (log/fatal nickname "ingress thread is dead" total))]

               [(eq? flavor 'ZYRE/EXIT)
                (log/info nickname "saw EXIT" total)
                (unless  (thread-send ingress (vector-immutable 'EXIT sender) #f)
                  (log/fatal nickname "ingress is dead" total))
                (zyre/event/destroy e)]

               [else ; Ignore JOIN, LEAVE, and SHOUT events.
                (zyre/event/destroy e)]))
           (loop (add1 traffic) (add1 total))]; snooze)]
          
          [else ; No more ZyRE events at this point.
           ; Reduce the snnoze time if one or more ZyRE events were seen this round
           ; and increase the snooze time if no ZyRE events were seen this round.
           (cond
             [(zpoller/expired? poller)
              (log/info nickname "poller expired" #t)]
             [(zpoller/terminated? poller)
              (log/info nickname "poller terminated" #t)])
           (sleep 0)])))))

;           (let ([snooze (snooze/adjust traffic snooze)])
;             (sleep 0) ;(sleep (/ snooze 1000.0)) ; Force the bridge thread to yield to other threads.
;             (loop 0 total snooze))])))))

#|
;; Given a zyre node z return an input port connected to the input side
;; of the pipe that connects the node to the underlying zyre node.
(define (zyre/port/input z)
  ;(racket/port/unix/input/new (zsocket/fd (zyre/socket z)) 'ZyRE))
  (let* ([zsocket (zyre/socket z)]
         [fd (zsocket/fd zsocket)])
    (displayln fd)
    (let-values([(in out) (racket/port/unix/both/new fd 'ZyRE)])
      (displayln in)
      (displayln out)
      in)))

;A bridge between the underlying ZyRE node of the island and the island itself.
;   z - the underlying ZyRE node of the island
;   inbound - the island thread that handles incoming ZyRE messages.

;Things are a little complicated because we have to mediate between two distinct threading models:
;the ZyRE model based on POSIX threads and the green threads model of Racket.
;We can't just use zpoller from CZMQ with a timeout because that would block the primary Racket POSIX thread
;(the one animating all of the Racket green threads) in the C code of CZMQ and bring everything at Racket
;level to a complete halt over the duration of the timeout (assuming no inbound ZyRE messages arrive
;in that period).
(define (zyre-to-island z inbound)
  (let ([port   (zyre/port/input z)] ; A Racket input port corresponding to the incoming side of ZyRE pipe.
        [poller (zpoller/new (zyre/socket z))])
    (displayln (format "zyre-to-island: ~a ~a" port poller))
    ; Block waiting for incoming bytes from the ZyRE layer.
    ; At this point we may not have a complete ZyRE message.
    (let loop/racket ([_ (sync port)])
      (displayln "zyre-to-island: port sees bytes")
      ; Check for complete ZyRE message. 0 means nonblocking.
      ; ZeroMQ polling is edge-triggered (see http://funcptr.net/2012/09/10/zeromq---edge-triggered-notification/)
      ; so that once it triggers we must repeatedly poll to pick up all waiting messages.
      (let loop/zyre ((go (zpoller/wait poller 0)))
        (when go
          (let* ([e (zyre/event/receive z)]
                 [flavor (zyre/event/flavor e)]
                 [sender (zyre/event/sender e)]) ; UUID of transmitting ZyRE node.
            (cond
              [(eq? flavor 'ZYRE/WHISPER) (displayln "saw WHISPER")]
;               (let* ([frame (zmessage/pop (zyre/event/message e))]
;                      [blob (zframe/payload frame)])
;                 (thread-send inbound (island/whisper sender blob))
;                 (zframe/destroy frame))]
              [(eq? flavor 'ZYRE/ENTER) (displayln "saw ENTER")]
;               (let ((headers (zhash-to-hash/persist (zyre/event/headers e))))
;                 (thread-send inbound (island/enter sender headers)))]
              [(eq? flavor 'ZYRE/EXIT) (displayln "saw EXIT")]
;               (thread-send inbound (island/exit sender))]
              [else #f]) ; Ignore JOIN, LEAVE, and SHOUT events.
            (zyre/event/destroy e))
          ; Try again.
          (loop/zyre (zpoller/wait poller 0))))
      ; No more complete ZyRE messages. Block waiting for incoming bytes from the ZyRE layer.
      (loop/racket (sync port)))))


(define (test01)
    (let* ([context (zcontext/new)]
         [alice (zyre/new context)]
         [bob (zyre/new context)])
      (zyre/port/input alice)
      (zyre/port/input bob)))


(define (test02)
  (let* ([context (zcontext/new)]
         [alice (zyre/new context)]
         [bob (zyre/new context)])
    (zyre/start alice)
    (zyre/start bob)
    (thread (lambda () (zyre-to-island alice #f)))))
|#

(define (test03)
  (let* ([context (zcontext/new)]
         [alice (zyre/new context)]
         [bob (zyre/new context)]
         [ingress (thread
                   (lambda ()
                     (let loop ([_ (thread-receive)]) (loop (thread-receive)))))])
    (zyre/start alice)
    (zyre/start bob)
    ; Shut bob down after three seconds.
    (thread (lambda () (sleep 3.0) (zyre/stop bob)))
    (list
     (thread (bridge/worker))
     (thread (bridge/worker)))))

|#


         
    