#lang racket/base

(require ffi/unsafe)

(provide
 zmq
 define-zmq-constant
 define-zmq-function
 zmq/version)

;; Find the latest version of the shared library libzmq.
(define zmq (ffi-lib "libzmq"))

;; zmq constants.
(define-syntax-rule (define-zmq-constant binding name type) ; type is usually _int or _bytes
  (define binding (get-ffi-obj name zmq (_fun -> type))))

;; Shorthand for binding zmq functions.
(define-syntax-rule (define-zmq-function binding name type)
  (define binding (get-ffi-obj name zmq type)))

;; Version information.
(define-zmq-function
  zmq/version "zmq_version"
  (_fun (major : (_ptr o _int)) (minor : (_ptr o _int)) (patch : (_ptr o _int))
   -> (r : _void)
   -> (values major minor patch)))

