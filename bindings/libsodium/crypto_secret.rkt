#lang racket/base

(require
 ffi/unsafe
 ;racket/contract
 (only-in "sodium.rkt" sodium define-sodium-function define-sodium-constant)
 (only-in "random.rkt" random/bytes/n))

(provide
 crypto/secret/key/size
 crypto/secret/nonce/size
 crypto/secret/primitive
 crypto/secret/key
 crypto/secret/encrypt
 crypto/secret/decrypt)


;; Authenticated symmetric encryption.

;; Pull the crypto/secret constants out of libsodium.
(define-sodium-constant crypto/secret/key/size           "crypto_secretbox_keybytes"     _int)
(define-sodium-constant crypto/secret/nonce/size         "crypto_secretbox_noncebytes"   _int)
(define-sodium-constant crypto/secret/plain/prefix/size  "crypto_secretbox_zerobytes"    _int)
(define-sodium-constant crypto/secret/cipher/prefix/size "crypto_secretbox_boxzerobytes" _int)
(define-sodium-constant crypto/secret/primitive          "crypto_secretbox_primitive"    _bytes)

(define CRYPTO/SECRET/KEY/SIZE           (crypto/secret/key/size))           ; Length of a symmetric key in bytes.
(define CRYPTO/SECRET/NONCE/SIZE         (crypto/secret/nonce/size))         ; Length of a nonce in bytes.
(define CRYPTO/SECRET/PLAIN/PREFIX/SIZE  (crypto/secret/plain/prefix/size))  ; Length of plaintext zero-filled prefix in bytes.
(define CRYPTO/SECRET/CIPHER/PREFIX/SIZE (crypto/secret/cipher/prefix/size)) ; Length of ciphertext zero-filled prefix in bytes.


(define (crypto/secret/key) (random/bytes/n CRYPTO/SECRET/KEY/SIZE))

;; int crypto_secretbox(unsigned char*       c,        // Ciphertext (output).
;;                      const unsigned char* m,        // Plaintext.
;;                      unsigned long long   m_length, // Length of plaintext in bytes.
;;                      const unsigned char* n,        // Nonce for ciphertext.
;;                      const unsigned char* k)        // Symmetric encryption key.
(define-sodium-function crypto-secret-box "crypto_secretbox" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

(define (crypto/secret/encrypt m n k)
  ; Input sanity.
;  (when (not (= (bytes-length k) CRYPTO/SECRET/KEY/SIZE))
;    (error 'crypto/secret/box "incorrect secret key length"))
;  (when (not (= (bytes-length n) CRYPTO/SECRET/NONCE/SIZE))
;    (error 'crypto/secret/box "incorrect nonce length"))

  (let* ((m/pad/length (+ (bytes-length m) CRYPTO/SECRET/PLAIN/PREFIX/SIZE))
         (m/pad (make-bytes m/pad/length 0))
         (c     (make-bytes m/pad/length 0)))
    (bytes-copy! m/pad CRYPTO/SECRET/PLAIN/PREFIX/SIZE m)
    (crypto-secret-box c m/pad m/pad/length n k)
    (subbytes c CRYPTO/SECRET/CIPHER/PREFIX/SIZE)))
    
;; int crypto_secretbox_open(unsigned char*       m,        // Plaintext (output).
;;                           const unsigned char* c,        // Ciphertext.
;;                           unsigned long long   c_length, // Length of ciphertext in bytes.
;;                           const unsigned char* n,        // Nonce.
;;                           const unsigned char* k)        // Symmetric encryption key.
(define-sodium-function crypto-secret-unbox "crypto_secretbox_open" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

(define (crypto/secret/decrypt c n k)
  ; Input sanity.
;  (when (not (= (bytes-length k) CRYPTO/SECRET/KEY/SIZE))
;    (error 'crypto/secret/unbox "incorrect secret key length"))
;  (when (not (= (bytes-length n) CRYPTO/SECRET/NONCE/SIZE))
;    (error 'crypto/secret/unbox "incorrect nonce length"))

  (let ((c/length (bytes-length c)))
;    (when (< c/length CRYPTO/SECRET/PLAIN/PREFIX/SIZE)
;      (error 'crypto/secret/unbox "ciphertext too short"))
    (let* ((c/pad/length (+ c/length CRYPTO/SECRET/CIPHER/PREFIX/SIZE))
           (c/pad (make-bytes c/pad/length 0))
           (m/pad (make-bytes c/pad/length 0)))
      (bytes-copy! c/pad CRYPTO/SECRET/CIPHER/PREFIX/SIZE c)
      (crypto-secret-unbox m/pad c/pad c/pad/length n k)
      (subbytes m/pad CRYPTO/SECRET/PLAIN/PREFIX/SIZE))))