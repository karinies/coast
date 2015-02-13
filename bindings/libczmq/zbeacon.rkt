#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(define-czmq-function zbeacon/new "zbeacon_new" (_fun (port : _int) -> _zbeacon))
(define-czmq-function zbeacon/destroy "zbeacon_destroy" (_fun (_ptr i _zbeacon) -> _void))
(define-czmq-function zbeacon/hostname "zbeacon_hostname" (_fun _zbeacon -> _string))