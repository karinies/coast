#lang racket/base

(require
 "../libczmq/czmq.rkt"
 "zyre.rkt")

(provide
 ENTER
 ENTER?
 ENTER/peer
 ENTER/destroy
 (rename-out [ENTER-headers ENTER/headers])
 
 EXIT
 EXIT/peer
 EXIT/destroy

 JOIN
 JOIN/peer
 JOIN/group
 JOIN/destroy
 
 LEAVE
 LEAVE/peer
 LEAVE/group
 LEAVE/destroy
 
 WHISPER
 WHISPER/peer
 WHISPER/destroy
 
 SHOUT
 SHOUT/peer
 SHOUT/group
 SHOUT/payload
 SHOUT/destroy
 
 zyre/message/unpack)

#|
command - a symbol,
headers - an association list of key/value pairs where both keys and values are Racket strings and
peer - byte string
group - byte string
payload - byte string
In each of the three, peer, group and payload,the byte string is OWNED by either zyre or czmq.
|#

;; A peer has entered the network.
(struct ENTER   (command (peer #:mutable) (headers #:mutable)) #:transparent)
;; A peer has exited the network.
(struct EXIT    (command (peer #:mutable)) #:transparent)
;; A peer has joined the given group.
(struct JOIN    (command (peer #:mutable) (group #:mutable)) #:transparent)
;; A peer has left the given group.
(struct LEAVE   (command (peer #:mutable) (group #:mutable)) #:transparent)
;; A peer has transmitted the given payload to you.
(struct WHISPER (command (peer #:mutable) (payload #:mutable)) #:transparent)
;; A peer has transmitted the given payload to every member of the given group.
(struct SHOUT   (command (peer #:mutable) (group #:mutable) (payload #:mutable)) #:transparent)

#|
WARNING! DANGER!
For all of the structures defined above the content of each peer, group, and payload
field is a byte string that is OWNED by either the zyre layer or the czmq layer below zyre.
If you want to retain ANY of these field values elsewhere you MUST copy the value using either
bytes-copy or bytes->bytes-immutable.

Secondly, these structures must be garbage-collected by hand (using the structure-specific */destroy function)
as the memory their fields refer to was allocated by either zyre or czmq and NOT Racket Scheme.
Once a structure has been handed over to its */destroy function it and its fields must never be referenced again.

Basically, don't use these structures unless you know exactly what you are doing.
They are a critical bridge between zyre and the communications layer of Island.
|#

;; Unpack a zyre ENTER message into a Racket structure.
(define (ENTER/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"ENTER"
         (frame2 (zmessage/pop m)) ; Peer UUID.
         (frame3 (zmessage/pop m)) ; Peer headers as packed zhash table.
         (h (zhash/unpack frame3))
         (enter
          (ENTER 'ENTER frame2 (zhash=>list h))))
    (zhash/destroy h)
    (zframe/destroy frame1) ; #"ENTER"
    (zframe/destroy frame3) ; Packed zhash table.
    (zmessage/destroy m)
    enter))

(define (ENTER/peer x) (zframe/payload (ENTER-peer x)))
(define (ENTER/headers x) (ENTER-headers x))
(define (ENTER/destroy x)
  (zframe/destroy (ENTER-peer x)) ; Peer UUID.
  (set-ENTER-peer! x #f)
  (set-ENTER-headers! x #f))

(define (EXIT/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"EXIT"
         (frame2 (zmessage/pop m)) ; UUID of departing peer.
         (exit (EXIT 'EXIT frame2)))
    (zframe/destroy frame1)
    (zmessage/destroy m)
    exit))
(define (EXIT/peer x) (zframe/payload (EXIT-peer x)))
(define (EXIT/destroy x)
  (zframe/destroy (ENTER-peer x))
  (set-EXIT-peer! x #f))

(define (WHISPER/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"WHISPER".
         (frame2 (zmessage/pop m)) ; Peer UUID.
         (frame3 (zmessage/pop m)) ; Message proper.
         (whisper (WHISPER 'WHISPER frame2 frame3)))
    (zframe/destroy frame1)
    (zmessage/destroy m)
    whisper))

(define (WHISPER/peer w)    (zframe/payload (WHISPER-peer w)))
(define (WHISPER/payload w) (zframe/payload (WHISPER-payload w)))
(define (WHISPER/destroy w)
  (zframe/destroy (WHISPER-peer w))
  (zframe/destroy (WHISPER-payload w))
  (set-WHISPER-peer!    w #f)
  (set-WHISPER-payload! w #f))

(define (SHOUT/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"SHOUT".
         (frame2 (zmessage/pop m)) ; Peer UUID.
         (frame3 (zmessage/pop m)) ; Group name.
         (frame4 (zmessage/pop m)) ; Message proper.
         (shout (SHOUT 'SHOUT frame2 frame3 frame4)))
    (zframe/destroy frame1)
    (zmessage/destroy m)
    shout))

(define (SHOUT/peer  x)   (zframe/payload (SHOUT-peer x)))
(define (SHOUT/group x)   (zframe/payload (SHOUT-group x)))
(define (SHOUT/payload x) (zframe/payload (SHOUT-payload x)))
(define (SHOUT/destroy x)
  (zframe/destroy (SHOUT-peer x))
  (zframe/destroy (SHOUT-group x))
  (zframe/destroy (SHOUT-payload x))
  (set-SHOUT-peer!    x #f)
  (set-SHOUT-group!   x #f)
  (set-SHOUT-payload! x #f))

(define (JOIN/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"JOIN".
         (frame2 (zmessage/pop m)) ; Peer UUID.
         (frame3 (zmessage/pop m)) ; Group name.
         (join (JOIN 'JOIN frame2 frame3)))
    (zframe/destroy frame1)
    (zmessage/destroy m)
    join))

(define (JOIN/peer x)  (zframe/payload (JOIN-peer  x)))
(define (JOIN/group x) (zframe/payload (JOIN-group x)))
(define (JOIN/destroy x)
  (zframe/destroy (JOIN-peer x))
  (zframe/destroy (JOIN-group x))
  (set-JOIN-peer!  x #f)
  (set-JOIN-group! x #f))

(define (LEAVE/new m)
  (let* ((frame1 (zmessage/pop m)) ; #"LEAVE"
         (frame2 (zmessage/pop m)) ; Peer UUID.
         (frame3 (zmessage/pop m)) ; Group name.
         (leave (LEAVE 'LEAVE frame2 frame3)))
    (zframe/destroy frame1)
    (zmessage/destroy m)
    leave))

(define (LEAVE/peer  x) (zframe/payload (LEAVE-peer  x)))
(define (LEAVE/group x) (zframe/payload (LEAVE-group x)))
(define (LEAVE/destroy x)
  (zframe/destroy (LEAVE-peer x))
  (zframe/destroy (LEAVE-group x))
  (set-LEAVE-peer!  x #f)
  (set-LEAVE-group! x #f)) 

(define (zyre/message/unpack m)
  (let ((frame1 (zmessage/frame/first m)))
    (case (zframe/payload frame1)
      ((#"WHISPER") (WHISPER/new m))
      ((#"SHOUT")   (SHOUT/new m))
      ((#"ENTER")   (ENTER/new m))
      ((#"EXIT")    (EXIT/new m))
      ((#"JOIN")    (JOIN/new m))
      ((#"LEAVE")   (LEAVE/new m))
      (else #f))))

(define (WHISPER/send self to payload)
  (let ((m (zmessage/new)))
    (zmessage/append* m to)
    (zmessage/append* m (zframe/new payload))
    (zyre/whisper self m)))
        
(define (zyre/headers/find headers key default)
  (let ((pair (assoc key headers)))
    (if pair (cdr pair) default)))
