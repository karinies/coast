#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function))

(provide crypto/hash)

;; Hashing.

(define CRYPTO/HASH/SIZE 64) ; Length in bytes of computed hash.

;; int crypto_hash(unsigned char*       h,        // Hash (output).
;;                 const unsigned char* m,        // Message.
;;                 unsigned long long   m_length) // Message length in bytes.
(define-sodium-function crypto-hash "crypto_hash" (_fun _bytes _bytes _ullong -> _int))

(define (crypto/hash m)
  (let ((hash (make-bytes CRYPTO/HASH/SIZE 0)))
    (crypto-hash hash m (bytes-length m))
    hash))
  