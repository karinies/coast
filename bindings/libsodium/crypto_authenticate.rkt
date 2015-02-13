#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function define-sodium-constant))

(provide
 crypto/authenticate/token/size
 crypto/authenticate/key/size
 crypto/authenticate/token crypto/authenticate?)

;; Authentication.

;; Pull the crypto/authenticate constants out of libsodium.
(define-sodium-constant crypto/authenticate/token/size "crypto_auth_bytes"     _int)
(define-sodium-constant crypto/authenticate/key/size   "crypto_auth_keybytes"  _int)
(define-sodium-constant crypto/authenticate/primitive  "crypto_auth_primitive" _bytes)

(define CRYPTO/AUTHENTICATE/TOKEN/SIZE (crypto/authenticate/token/size)) ; Length in bytes of the authentication token.
(define CRYPTO/AUTHENTICATE/KEY/SIZE   (crypto/authenticate/key/size))   ; Length in bytes of the secret key.

;; int crypto_auth(unsigned char*       t,        // Authentication token (output).
;;                 const unsigned char* m,        // Message (either plaintext or ciphertext).
;;                 unsigned long long   m_length, // Message length in bytes.
;;                 const unsigned char* k)        // Shared secret key.
(define-sodium-function crypto-auth "crypto_auth" (_fun _bytes _bytes _ullong _bytes -> _int))

(define (crypto/authenticate/token m k)
;  (when (not (= (bytes-length k) CRYPTO/AUTHENTICATE/KEY/SIZE))
;    (error 'crypto/authenticate/token/new "incorrect secret key length"))
  (let ((token (make-bytes CRYPTO/AUTHENTICATE/TOKEN/SIZE 0)))
    (crypto-auth token m (bytes-length m) k)
    token))

;; int crypto_auth_verify(const unsigned char* t,        // Authentication token.
;;                        const unsigned char* m,        // Material to be authenticated.
;;                        unsigned long long   m_length, // Material length in bytes.
;;                        const unsigned char* k)        // Shared secret key.
(define-sodium-function crypto-auth-verify "crypto_auth_verify" (_fun _bytes _bytes _ullong _bytes -> _int))

(define (crypto/authenticate? token m k)
  ; Input sanity.
;  (when (not (= (bytes-length k) CRYPTO/AUTHENTICATE/KEY/SIZE))
;    (error 'crypto/authenticate/verify "incorrect secret key length"))
;  (when (not (= (bytes-length token) CRYPTO/AUTHENTICATE/TOKEN/SIZE))
;    (error 'crypto/authenticate/verify "incorrect token length"))
  
  (zero? (crypto-auth-verify token m (bytes-length m) k)))
