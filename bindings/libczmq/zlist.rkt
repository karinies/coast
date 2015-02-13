#lang racket/base

(require
 ffi/unsafe
 "libczmq.rkt")

(provide
 zlist/new
 zlist/destroy
 zlist/append
 zlist/first
 zlist/next
 zlist=>list)

;; NOTE: We assume that zlists contain strings and never anything else because
;; it is impossible at C level to guess the type of a list item value.

;; The two functions zlist/first and zlist/next are not reentrant since they (re)set
;; a cursor field in a CZMQ zlist structure.
(define-czmq-function zlist/first "zlist_first" (_fun _zlist -> _string))
(define-czmq-function zlist/next  "zlist_next"  (_fun _zlist -> _string))

(define-czmq-function zlist/new "zlist_new" (_fun -> _zlist))
(define-czmq-function zlist/destroy "zlist_destroy" (_fun (_ptr i _zlist) -> _void))
(define-czmq-function zlist/append "zlist_append" (_fun _zlist _string -> (r : _int) -> (zero? r)))

(define (zlist=>list x)
  (let loop ((item (zlist/first x)) (output null))
    (if item
        (loop (zlist/next x) (cons item output))
        (reverse output))))

(define (zlist/test)
  (let ((x (zlist/new)))
    (displayln (zlist=>list x)) ; Empty list.
    (zlist/append x "foo")
    (zlist/append x "bar")
    (zlist/append x "nix")
    (displayln (zlist=>list x)) ; Non-empty list.
    (zlist/destroy x)))