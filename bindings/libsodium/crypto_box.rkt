#lang racket/base

(require
 ffi/unsafe
 (only-in "sodium.rkt" sodium define-sodium-constant define-sodium-function))

(provide
 crypto/box/keys
 crypto/box/encrypt
 crypto/box/decrypt
 crypto/box/precompute
 crypto/box/pre/encrypt
 crypto/box/pre/decrypt
 crypto/box/key/size
 crypto/box/secret/size
 crypto/box/nonce/size
 crypto/box/primitive
 CRYPTO/BOX/KP/SIZE
 CRYPTO/BOX/KS/SIZE)

;; Public/secret key encryption.

(define-sodium-constant crypto/box/key/size           "crypto_box_publickeybytes" _int) ; Size of a public key in bytes.
(define-sodium-constant crypto/box/secret/size        "crypto_box_secretkeybytes" _int) ; Size of a secret key in bytes.
(define-sodium-constant crypto/box/precompute/size    "crypto_box_beforenmbytes"  _int) ; Size in bytes of precomputed keying information.
(define-sodium-constant crypto/box/nonce/size         "crypto_box_noncebytes"     _int) ; Size of nonce in bytes.
(define-sodium-constant crypto/box/plain/prefix/size  "crypto_box_zerobytes"      _int) ; Number of leading 0 bytes in a message (plaintext)
(define-sodium-constant crypto/box/cipher/prefix/size "crypto_box_boxzerobytes"   _int) ; Number of leading 0 bytes in a ciphertext.
(define-sodium-constant crypto/box/primitive          "crypto_box_primitive"    _bytes) ; ASCII description of underlying implementation.

;; Pull the crypto/box implementation constants out of libsodium.
(define CRYPTO/BOX/KP/SIZE            (crypto/box/key/size))           ; Number of bytes in a public key.
(define CRYPTO/BOX/KS/SIZE            (crypto/box/secret/size))        ; Number of bytes in a secret key.
(define CRYPTO/BOX/PLAIN/PREFIX/SIZE  (crypto/box/plain/prefix/size))  ; Number of leading 0 bytes (padding) in a message.
(define CRYPTO/BOX/CIPHER/PREFIX/SIZE (crypto/box/cipher/prefix/size)) ; Number of leading 0 bytes (padding) in a ciphertext.
(define CRYPTO/BOX/NONCE/SIZE         (crypto/box/nonce/size))         ; Number of bytes in a nonce.
(define CRYPTO/BOX/PRECOMPUTE/SIZE    (crypto/box/precompute/size))    ; Number of bytes in precomputed public/secret shortcut.

;; Public/secret key encryption.

;; A minor note of terminology. NaCl (and libsodium which duplicates the NaCl API) uses "box" as a verb
;; in the sense of "boxing" a message (plaintext) into an encrypted blob (ciphertext).

;; Randomly generate a public key/secret key pair,
;; CRYPTO/BOX/KP/SIZE and CRYPTO/BOX/KS/SIZE long respectively.
;; Returns 0 if key generation succeeded.
;; int crypto_box_keypair(unsigned char *pk, unsigned char *sk)
(define-sodium-function crypto-box-key-pair "crypto_box_keypair" (_fun _bytes _bytes -> _int))

;; Return a fresh public key/secret key pair.
(define (crypto/box/keys)
  (let ((pk (make-bytes CRYPTO/BOX/KP/SIZE    0))
        (sk (make-bytes CRYPTO/BOX/KS/SIZE 0)))
    (crypto-box-key-pair pk sk)
    (values pk sk)))
       

;; Encrypts a message m given the sender's secret key sk and the receivers public key pk
;; using nonce n. The result is ciphertext c. The length in bytes of both c and m is mlen.
;; Returns 0 iff successful.
;; int crypto_box(unsigned char*       c,
;;                const unsigned char* m,
;;                unsigned long long   m_length,
;;                const unsigned char* n,
;;                const unsigned char* pk,
;;                const unsigned char* sk)
(define-sodium-function crypto-box "crypto_box"
  (_fun _bytes _bytes _ullong _bytes _bytes _bytes -> _int))

;; Encrypts a message m given the sender's secret key sk and the receivers public key pk
;; using nonce n. Returns the ciphertext for the message m.
;; All of the inputs are byte strings.
;; This function is modeled after the equivalent C++ function in NaCL.
(define (crypto/box/encrypt m n pk sk)
  ; Input sanity.
;  (when (not (= (bytes-length pk) CRYPTO/BOX/KP/SIZE))
;    (error 'crypto/box "incorrect public key length"))
;  (when (not (= (bytes-length sk) CRYPTO/BOX/KS/SIZE))
;    (error 'crypto/box "incorrect secret key length"))
;  (when (not (= (bytes-length n) CRYPTO/BOX/NONCE/SIZE))
;    (error 'crypto/box "incorrect nonce length"))

  (let* ((m/pad/length (+ (bytes-length m) CRYPTO/BOX/PLAIN/PREFIX/SIZE))
         (m/pad (make-bytes m/pad/length 0))
         (c     (make-bytes m/pad/length 0))) ; 
    (bytes-copy! m/pad CRYPTO/BOX/PLAIN/PREFIX/SIZE m) ; Hence the first CRYPTO/BOX/PLAIN/PREFIX/SIZE of m are 0.
    (when (not (zero? (crypto-box c m/pad m/pad/length n pk sk)))
      (error 'crypto/box "unable to box (encrypt) a message"))
    (subbytes c CRYPTO/BOX/CIPHER/PREFIX/SIZE)))

;; Decrypt "boxed" ciphertext c into plaintext message m using the public key of the sender (for authentication)
;; and the secret key of the receiver (for decryption).
;; int crypto_box_open(unsigned char*       m,
;;                     const unsigned char* c,
;;                     unsigned long long   c_length,
;;                     const unsigned char* n,
;;                     const unsigned char* pk,
;;                     const unsigned char* sk);
(define-sodium-function crypto-box-open "crypto_box_open" (_fun _bytes _bytes _ullong _bytes _bytes _bytes -> _int))

;; Decrypt ciphertext c with nonce n, the public key pk of the sender and the secret key sk
;; of the receiver. Returns the plaintext as bytes if successful.
;; All of the inputs are byte strings.
;; This function is modeled after the equivalent C++ function in NaCL.
(define (crypto/box/decrypt c n pk sk)
  ; Input sanity.
;  (when (not (= (bytes-length pk) CRYPTO/BOX/KP/SIZE))
;    (error 'crypto/unbox "incorrect public key length"))
;  (when (not (= (bytes-length sk) CRYPTO/BOX/KS/SIZE))
;    (error 'crypto/unbox "incorrect secret key length"))
;  (when (not (= (bytes-length n) CRYPTO/BOX/NONCE/SIZE))
;    (error 'crypto/unbox "incorrect nonce length"))

  (let* ((c/pad/length (+ (bytes-length c) CRYPTO/BOX/CIPHER/PREFIX/SIZE))
         (c/pad (make-bytes c/pad/length 0))
         (m/pad (make-bytes c/pad/length 0)))
;    (when (< c/pad/length CRYPTO/BOX/PLAIN/PREFIX/SIZE)
;      (error 'crypto/unbox "ciphertext too short"))

    (bytes-copy! c/pad CRYPTO/BOX/CIPHER/PREFIX/SIZE c)
    (when (not (zero? (crypto-box-open m/pad c/pad c/pad/length n pk sk)))
      (error 'crypto/unbox "ciphertext fails verification"))
    (subbytes m/pad CRYPTO/BOX/PLAIN/PREFIX/SIZE)))

;; Precompute the encryption/signing information as an optimization.
;; int crypto_box_beforenm(unsigned char*       k,  ; Precomputed encryption/signing information (output).
;;                         const unsigned char* pk, ; Receiver public key (for encryption).
;;                         const unsigned char* sk) ; Sender secret key (for signing).
;; The return value is always 0 and can be ignored.
(define-sodium-function crypto-box-before "crypto_box_beforenm" (_fun _bytes _bytes _bytes -> _int))

;; Precomputes information required for encryption and decryption with
;; receiver's public key pk (for encryption) and sender's secret key sk (for signing).
;; Returns the precomputed information as a 32-byte string.
(define (crypto/box/precompute pk sk)
  ; Input sanity.
;  (when (not (= (bytes-length pk) CRYPTO/BOX/KP/SIZE))
;    (error 'crypto/box/precompute "incorrect public key length"))
;  (when (not (= (bytes-length sk) CRYPTO/BOX/KS/SIZE))
;    (error 'crypto/box/precompute "incorrect secret key length"))
  (let ((k (make-bytes CRYPTO/BOX/PRECOMPUTE/SIZE 0)))
    (crypto-box-before k pk sk)
    k))

;; Encrypt message m into ciphertext c using nonce n and the precomputed keying information k.
;; int crypto_box_afternm(unsigned char*       c,        ; Ciphertext output.
;;                        const unsigned char* m,        ; Plaintext message.
;;                        unsigned long long   m_length, ; Message (and ciphertext) length in bytes.
;;                        const unsigned char* n,        ; Nonce for message.
;;                        const unsigned char* k)        ; Precomputed encryption/signing information.
(define-sodium-function crypto-box-after "crypto_box_afternm" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

;; Identical in effect to crypto/box except that it uses the precomputed encryption/signing information.
(define (crypto/box/pre/encrypt m nonce pre)
  ; Input sanity.
;  (when (not (= (bytes-length nonce) CRYPTO/BOX/NONCE/SIZE))
;    (error 'crypto/box/pre "incorrect nonce length"))  
;  (when (not (= (bytes-length pre) CRYPTO/BOX/PRECOMPUTE/SIZE))
;    (error 'crypto/box/pre "incorrect precompute length"))

  (let* ((m/pad/length (+ (bytes-length m) CRYPTO/BOX/PLAIN/PREFIX/SIZE))
         (m/pad (make-bytes m/pad/length 0))
         (ciphertext (make-bytes m/pad/length 0)))
    (bytes-copy! m/pad CRYPTO/BOX/PLAIN/PREFIX/SIZE m) ; Hence the first CRYPTO/BOX/PLAIN/PREFIX/SIZE of m are 0.
    (when (not (zero? (crypto-box-after ciphertext m/pad m/pad/length nonce pre)))
      (error 'crypto/box/pre "unable to box (encrypt) a message"))
    (subbytes ciphertext CRYPTO/BOX/CIPHER/PREFIX/SIZE))) ; Retun the ciphertext.

;; int crypto_box_open_afternm(unsigned char*       m,        ; Plaintext output.
;;                             const unsigned char* c,        ; Ciphertext
;;                             unsigned long long   c_length, ; Ciphertext (and message) length in bytes.
;;                             const unsigned char* n,        ; Nonce for message.
;;                             const unsigned char* k)        ; Precomputed encryption/signing information.
(define-sodium-function crypto-box-open-after "crypto_box_open_afternm" (_fun _bytes _bytes _ullong _bytes _bytes -> _int))

(define (crypto/box/pre/decrypt c n pre)
  ; Input sanity.
;  (when (not (= (bytes-length n) CRYPTO/BOX/NONCE/SIZE))
;    (error 'crypto/unbox/pre "incorrect nonce length"))
;  (when (not (= (bytes-length pre) CRYPTO/BOX/PRECOMPUTE/SIZE))
;    (error 'crypto/unbox/pre "incorrect precompute length"))
  
  (let* ((c/pad/length (+ (bytes-length c) CRYPTO/BOX/CIPHER/PREFIX/SIZE))
         (c/pad (make-bytes c/pad/length 0))
         (m/pad (make-bytes c/pad/length 0)))
;    (when (< c/pad/length CRYPTO/BOX/PLAIN/PREFIX/SIZE)
;      (error 'crypto/unbox/pre "ciphertext too short"))

    (bytes-copy! c/pad CRYPTO/BOX/CIPHER/PREFIX/SIZE c)
    (when (not (zero? (crypto-box-open-after m/pad c/pad c/pad/length n pre)))
      (error 'crypto/unbox/pre "ciphertext fails verification"))
    (subbytes m/pad CRYPTO/BOX/PLAIN/PREFIX/SIZE)))
