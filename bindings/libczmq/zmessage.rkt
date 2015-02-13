#lang racket/base

(require
 ffi/unsafe
 (only-in
  "zframe.rkt"
  zframe/new zframe/data zframe/destroy zframe/size)
 "libczmq.rkt")

(provide
 zmessage/new
 zmessage/destroy
 zmessage/receive
 zmessage/send
 zmessage/frames
 zmessage/size
 zmessage/push
 zmessage/append
 zmessage/pop
 zmessage/push*
 zmessage/append*
 zmessage/pop*
 zmessage/wrap
 zmessage/unwrap
 zmessage/duplicate
 zmessage/frame/first
 zmessage/frame/last
 zmessage/frame/next
 zmessage/string/add
 zmessage/string/push
 zmessage/string/pop
 zmessage/debug)

;; Create an empty message object.
(define-czmq-function zmessage/new "zmsg_new" (_fun -> _zmessage))

;; Destroy a message object and all of its constituent frames.
(define-czmq-function zmessage/destroy "zmsg_destroy" (_fun (_ptr i _zmessage) -> _void))

;; Blocking receive from Zeromq socket.
;; Note: Direct call from a Racket thread will freeze Racket since the Racket thread will block
;; itself in the C level preventing the Racket thread scheduler from advancing.
(define-czmq-function zmessage/receive "zmsg_recv" (_fun _zsocket -> _zmessage))

;; Send a message to a socket and then destroy the message.
(define-czmq-function zmessage/send "zmsg_send" (_fun (_ptr i _zmessage) _zsocket -> (r : _int = (zero? r))))

;; Number of frames in the given message.
(define-czmq-function zmessage/frames "zmsg_size" (_fun _zmessage -> _size_t))

;; Total size of message in bytes.
(define-czmq-function zmessage/size "zmsg_content_size" (_fun _zmessage -> _size_t))

;; Frame-wise message construction.
;; Push a zframe onto the given message.
(define-czmq-function zmessage/push "zmsg_push" (_fun _zmessage _zframe -> (r : _int = (zero? r))))
;; Append a zframe onto the given message.
(define-czmq-function zmessage/append "zmsg_append" (_fun _zmessage (_ptr i _zframe) -> (r : _int = (zero? r))))
;; Pop a zframe off of the message.
(define-czmq-function zmessage/pop "zmsg_pop" (_fun _zmessage -> _zframe))

(define (zmessage/push* m x)
  (cond
    ((zframe? x) (zmessage/push m x))
    ((bytes? x)  (zmessage/push m (zframe/new x)))
    ((string? x) (zmessage/string/push m x))))
                 
(define (zmessage/append* m x)
  (cond
    ((zframe? x) (zmessage/append m x))
    ((bytes? x)  (zmessage/append m (zframe/new x)))
    ((string? x) (zmessage/string/add m x))))

(define (zmessage/pop* m)
  (let* ((frame (zmessage/pop m))
         (b (and frame (zframe/data frame))))
    (when frame (zframe/destroy frame))
    b))
    
;; Push two zframes onto a message, first an empty zframe and then push the given frame.
(define-czmq-function zmessage/wrap "zmsg_wrap" (_fun _zmessage _zframe -> _void))
;; Pop zframe off of message and return it.
;; If the following zframe is empty then it is also popped and silently destroyed.
(define-czmq-function zmessage/unwrap "zmsg_unwrap" (_fun _zmessage -> _zframe))
;; Remove the given zframe from the message (if present). Does not destroy the zframe.
(define-czmq-function zmessage/remove "zmsg_remove" (_fun _zmessage _zframe -> _void))

;; Push a string as a new frame onto the front of a message.
(define-czmq-function zmessage/string/push "zmsg_pushstr" (_fun _zmessage _string -> (r : _int = (zero? r))))
;; Add a string as a new frame onto the end of a message.
(define-czmq-function zmessage/string/add "zmsg_addstr" (_fun _zmessage _string -> (r : _int = (zero? r))))
;; Pop a frame off the given message returning the frame as a string.
;; If no frames are left in the message returns #f.
(define-czmq-function zmessage/string/pop "zmsg_popstr" (_fun _zmessage -> (s : _string)))

;; Sets cursor to first frame in message.
;; Return the frame (if any) or #f is the message is empty.
(define-czmq-function zmessage/frame/first "zmsg_first" (_fun _zmessage -> (f : _zframe)))
;; Return the next frame in the message (if any) and advance the cursor.
;; Return #f if the cursor has advanced beyond the last frame.
(define-czmq-function zmessage/frame/next "zmsg_next" (_fun _zmessage -> (f : _zframe)))
;; Return the last frame of the the message.
;; Return #f if the message is empty.
(define-czmq-function zmessage/frame/last "zmsg_last" (_fun _zmessage -> (f : _zframe)))

;; Duplicate a message. Returns the fresh duplicate on success and #f
;; if insufficient memory is available.
(define-czmq-function zmessage/duplicate "zmsg_dup" (_fun _zmessage -> (m : _zmessage)))

;; Write a zmesg to an output port.
(define (zmessage/debug m port)
  (let loop ((frame (zmessage/frame/first m)))
    (when frame
      (display (format "~a ~a\n" (zframe/size frame) (zframe/data frame)) port)
      (loop (zmessage/frame/next m)))))