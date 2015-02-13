#lang racket/base

(require
 ffi/unsafe
 (only-in "../libzmq/zmq.rkt" zmq/assert/0 zmq/assert/0...)
 (only-in "../libczmq/libczmq.rkt" _zframe _zlist _zhash _zsocket)
 "libzyre.rkt")

;(define ZYRE/MESSAGE/HELLO   1)                                                                         
;(define ZYRE/MESSAGE/WHISPER 2)                                                                  
;(define ZYRE/MESSAGE/SHOUT   3)
;(define ZYRE/MESSAGE/JOIN    4)
;(define ZYRE/MESSAGE/LEAVE   5)
;(define ZYRE/MESSAGE/PING    6) 
;(define ZYRE/MESSAGE/PONG    7)

;(define-cpointer-type _zsocket) ; Used by czmq.

;; Create a new zre message.
(define-zyre-function zyre/message/new "zre_msg_new" (_fun (flavor : _int) -> _zyre-message))
;; Destroy the given zre message.
(define-zyre-function zyre/message/destroy "zre_msg_destroy" (_fun (_ptr i _zyre-message) -> _void))
;; Receive and parse a zre message.
(define-zyre-function zyre/message/receive "zre_msg_recv" (_fun _zsocket -> _zyre-message))
;; Send the given zre message to the output and destroy it.
(define-zyre-function
  zyre/message/send "zre_msg_send"
  (_fun
   (_ptr i _zyre-message) _zsocket
   -> (r : _int) -> (zmq/assert/0... 'zyre/message/send r)))

;; Send HELLO to the output in a single step.
(define-zyre-function
  zyre/hello/send "zre_msg_send_hello"
  (_fun
   _zsocket
   (sequence : _uint16)
   (address  : _string)
   (mailbox  : _uint16)
   (groups   : _zlist)
   (status   : _uint8)
   (headers  : _zhash)
   -> (r : _int) -> (zmq/assert/0... 'zyre/hello/send r)))

;; Send a WHISPER to the output in a single step.
(define-zyre-function zyre/whisper/send "zre_msg_send_whisper"
  (_fun _zsocket (sequence : _uint16) (content : _zframe)
        -> (r : _int) -> (zmq/assert/0... 'zyre/whisper/send r)))

;; Send a SHOUT to the output in a single step.
(define-zyre-function zyre/shout/send "zre_msg_send_shout"
  (_fun _zsocket (sequence : _uint16) (group : _string) (content : _zframe)
        -> (r : _int) -> (zmq/assert/0... 'zyre/shout/send r)))

;; Send a JOIN to the output in a single step.
(define-zyre-function zyre/join/send "zre_msg_send_join"
  (_fun _zsocket (sequence : _uint16) (group : _string) (status : _uint8)
        -> (r : _int) -> (zmq/assert/0... 'zyre/join/send r)))
 
;; Send a LEAVE to the output in a single step.
(define-zyre-function zyre/leave/send "zre_msg_send_leave"
  (_fun _zsocket (sequence : _uint16) (group : _string) (status : _uint8)
        -> (r : _int) -> (zmq/assert/0... 'zyre/leave/send r)))

;; Send a PING to the output in a single step.
(define-zyre-function zyre/ping/send "zre_msg_send_ping"
  (_fun _zsocket (sequence : _uint16)
        -> (r : _int) -> (zmq/assert/0... 'zyre/ping/send r)))

;; Send a PONG (acknowledgement of PING) to the output in a single step.
(define-zyre-function zyre/pong/send "zre_msg_send_ping_ok"
  (_fun _zsocket (sequence : _uint16)
        -> (r : _int) -> (zmq/assert/0... 'zyre/pong/send r)))

;; Duplicate a zyre message.
(define-zyre-function zyre/message/duplicate "zre_msg_dup" (_fun _zyre-message -> _zyre-message))

;; Dump the contents of a zyre message to standard out.
(define-zyre-function zyre/message/dump "zre_msg_dump" (_fun _zyre-message -> _void))

;; Get/set zyre message IP address.
(define-zyre-function zyre/message/address 'zre_msg_address (_fun _zyre-message -> _zframe))
(define-zyre-function zyre/message/address/set 'zre_msg_set_address (_fun _zyre-message (address : _zframe) -> _void))

;; Get/set zyre message id.
(define-zyre-function zyre/message/id "zre_msg_id" (_fun _zyre-message -> _int))
(define-zyre-function zyre/message/id/set "zre_msg_set_id" (_fun _zyre-message (id : _int) -> _void))

;; Get zyre message command as a string.
(define-zyre-function zyre/message/command "zre_msg_command" (_fun _zyre-message -> _string))

;; Get/set the zyre message IP address host:port.
(define-zyre-function zyre/message/to "zre_msg_ipaddress" (_fun _zyre-message -> _string))