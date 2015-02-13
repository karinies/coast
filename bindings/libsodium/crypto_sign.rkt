#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-function define-sodium-constant))

(provide
 CRYPTO/SIGN/KP/SIZE
 CRYPTO/SIGN/KS/SIZE
 crypto/sign/keys crypto/sign crypto/sign/verify
 crypto/sign/size
 crypto/sign/key/size
 crypto/sign/secret/size
 crypto/sign/primitive)

;; Digital signatures.


(define-cstruct _junk-ullong ((_ _ullong)))
(define JUNK-ULLONG-POINTER (make-junk-ullong 0))

; size_t crypto_sign_bytes(void)
(define-sodium-constant crypto/sign/size        "crypto_sign_bytes"          _int)  ; Length of signature in bytes.
; size_t crypto_sign_publickeybytes(void)
(define-sodium-constant crypto/sign/key/size    "crypto_sign_publickeybytes" _int)  ; Length of public signing key in bytes.
; size_t crypto_sign_secretkeybytes(void)
(define-sodium-constant crypto/sign/secret/size "crypto_sign_secretkeybytes" _int)  ; Length of secret signing key in bytes.
; const char* crypto_sign_primitive(void)
(define-sodium-constant crypto/sign/primitive   "crypto_sign_primitive"      _bytes) ; Name of underlying implementation as bytes string.

(define CRYPTO/SIGN/SIZE    (crypto/sign/size))        ; Number of bytes in a signature.
(define CRYPTO/SIGN/KP/SIZE (crypto/sign/key/size))    ; Number of bytes in verification key.
(define CRYPTO/SIGN/KS/SIZE (crypto/sign/secret/size)) ; Number of bytes in signing key.

;; Generate a public/secret key pair.
; int crypto_sign_keypair(unsigned char *pk, unsigned char* sk)
(define-sodium-function crypto-sign-key-pair "crypto_sign_keypair" (_fun _bytes _bytes -> _int))

;; Generate a verification/signing key pair returning them as (values pk sk).
(define (crypto/sign/keys)
  (let ((pk (make-bytes CRYPTO/SIGN/KP/SIZE    0))
        (sk (make-bytes CRYPTO/SIGN/KS/SIZE 0)))
    (crypto-sign-key-pair pk sk)
    (values pk sk)))

;; This is a special purpose wrapper around the libsodium crypto_sign to help prevent Racket from crashing.
;; int racket_crypto_sign(unsigned char*       s,        ; Signed message (output).
;;                        unsigned long long*  s_length  ; Signed message length in bytes.
;;                        const unsigned char* m,        ; Message.
;;                        unsigned long long   m_length, ; Length of message in bytes.
;;                        const unsigned char* sk)       ; Signing key.
(define-sodium-function crypto-sign "crypto_sign" (_fun _bytes _junk-ullong-pointer _bytes _ullong _bytes -> _int))

;; Signs message m using signing key sk.
;; Returns a signed message where the first CRYPTO/SIGN/SIZE bytes are the signature and the rest are message m.
(define (crypto/sign m sk)
;  (when (not (= (bytes-length sk) CRYPTO/SIGN/KS/SIZE))
;    (error 'crypto/sign "incorrect signing key length"))
  (let* ((m/length (bytes-length m))
         (signature (make-bytes (+ m/length CRYPTO/SIGN/SIZE) 0)))
    (crypto-sign signature JUNK-ULLONG-POINTER m m/length sk)
    signature))

;; int crypto_sign_open(unsigned char*        m,        ; Message (output).
;;                      unsigned long long*   m_length  ; Message length in bytes.
;;                      const unsigned char*  s,        ; Signed message.
;;                      unsigned long long    s_length, ; Length of signed message in bytes
;;                      const unsigned char*  pk)       ; Verification key.
(define-sodium-function crypto-sign-open "crypto_sign_open" (_fun _bytes _junk-ullong-pointer _bytes _ullong _bytes -> _int))

;; Given signed material signing and verification key pk return the original material if the signature is correct
;; and false otherwise.
(define (crypto/sign/verify signing pk)
;  (when (not (= (bytes-length pk) CRYPTO/SIGN/KP/SIZE))
;    (error 'crypto/unsign "incorrect verification key length"))
  (let ((signing/length (bytes-length signing)))
;    (when (< signing/length CRYPTO/SIGN/SIZE)
;      (error 'crypto/unsign "signed material is too short"))

    (let ((message (make-bytes (- signing/length CRYPTO/SIGN/SIZE) 0)))
      (if (zero? (crypto-sign-open message JUNK-ULLONG-POINTER signing signing/length pk))
          message
          #f))))