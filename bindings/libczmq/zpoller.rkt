#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt"
 "zcontext.rkt"
 "zsocket.rkt")

(provide
 zpoller/new
 zpoller/destroy
 zpoller/add
 zpoller/wait
 zpoller/expired?
 zpoller/terminated?)
 

(define-czmq-function zpoller/raw "zpoller_new" (_fun _zsocket/null -> _zpoller))
(define-czmq-function zpoller/destroy "zpoller_destroy" (_fun (_ptr i _zpoller) -> _void))
(define-czmq-function zpoller/add "zpoller_add" (_fun _zpoller _zsocket -> (r : _int = (zero? r))))
(define-czmq-function zpoller/wait "zpoller_wait" (_fun _zpoller _int -> _zsocket/null))
(define-czmq-function zpoller/expired? "zpoller_expired" (_fun _zpoller -> _bool))
(define-czmq-function zpoller/terminated? "zpoller_terminated" (_fun _zpoller -> _bool))

(define (zpoller/new . rest)
  (let ((p (zpoller/raw #f)))
    (let loop ((readers rest))
      (cond
        [(null? readers) p]
        [else
         (zpoller/add p (car readers))
         (loop (cdr readers))]))))


  