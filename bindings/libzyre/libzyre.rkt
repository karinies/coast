#lang racket/base

(require ffi/unsafe)

(provide
 zyre?
 define-zyre-constant
 define-zyre-function
 ; C pointer types.
 _zyre
 _zyre-event
 _zyre-log
 _zyre-node
 _zyre-message)


;; Find the lastest version of the shared library libzyre.
(define zyre (ffi-lib "libzyre"))

;; zyre constants.
(define-syntax-rule (define-zyre-constant binding name type)
  (define binding (get-ffi-obj name zyre (_fun -> type))))

;; Shorthand for binding zyre functions.
(define-syntax-rule (define-zyre-function binding name type)
  (define binding (get-ffi-obj name zyre type)))

(define-cpointer-type _zyre)
(define-cpointer-type _zyre-node)
(define-cpointer-type _zyre-message)
(define-cpointer-type _zyre-log)
(define-cpointer-type _zyre-event)


