#lang racket/base

(require
 ffi/unsafe
 "../libczmq/libczmq.rkt"
 "libzyre.rkt")

(provide
 zyre/event/destroy
 zyre/event/new
 zyre/event/flavor
 zyre/event/sender
 zyre/event/headers
 zyre/event/header
 zyre/event/group
 zyre/event/message)
 
(define-zyre-function zyre/event/destroy "zyre_event_destroy" (_fun (_ptr i _zyre-event) -> _void))
(define-zyre-function zyre/event/new "zyre_event_new" (_fun _zyre -> _zyre-event))
(define-zyre-function zyre/event/flavor "zyre_event_type"
  (_fun _zyre-event -> (_enum '(ZYRE/ENTER = 1 ZYRE/JOIN ZYRE/LEAVE ZYRE/EXIT ZYRE/WHISPER ZYRE/SHOUT))))
;; Returns sender UUID as 32 character ASCII hex.
(define-zyre-function zyre/event/sender "zyre_event_sender" (_fun _zyre-event -> (sender : _string)))
(define-zyre-function zyre/event/headers "zyre_event_headers" (_fun _zyre-event -> _zhash/null))
(define-zyre-function zyre/event/header "zyre_event_header" (_fun _zyre-event (key : _string) -> (value : _string)))
(define-zyre-function zyre/event/group "zyre_event_group" (_fun _zyre-event -> _string))
(define-zyre-function zyre/event/message "zyre_event_msg" (_fun _zyre-event -> _zmessage))
