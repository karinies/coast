#lang racket/base

(require ffi/unsafe)

(provide
 sodium
 define-sodium-constant
 define-sodium-function)

;; Find version 4 of the shared library libsodium.
(define sodium (ffi-lib "/usr/local/lib/libsodium" '("4" #f)))

;; Sodium constants.
(define-syntax-rule (define-sodium-constant binding name type) ; type is usually _int or _bytes
  (define binding (get-ffi-obj name sodium (_fun -> type))))

;; Shorthand for binding sodium functions.
(define-syntax-rule (define-sodium-function binding name type)
  (define binding (get-ffi-obj name sodium type)))

; Unused for now.
(define-sodium-function sodium/initialize "sodium_init"
  (_fun -> (r : _int) -> (when (not (zero? r)) (error 'libsodium "unable to initalize libsodium"))))

;; Version information.

(define-sodium-function sodium/version "sodium_version_string" (_fun -> _bytes))
; The following two haven't been implemented yet in libsodium.
(define-sodium-function sodium/version/major "sodium_library_version_major" (_fun -> _int))
(define-sodium-function sodium/version/minor "sodium_library_version_minor" (_fun -> _int))
